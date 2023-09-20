#lang racket/base

;; homebrew implementation of racket/cmdline
;; for more expressiveness, better help

(require
    "../utils/ann.rkt"
    "../utils.rkt"
    "../repl/utils.rkt"
    racket/list racket/sequence racket/string
    racket/match racket/block
    threading
    data/union-find
    (for-syntax racket/base
        syntax/parse
        threading
))

(:typedef FlagLiteral String)
(:typedef FlagDesc String)
(:typedef (FlagSpec a ...)
    (List (Listof FlagLiteral) (Listof FlagDesc) (a ... -> Void) (Listof String)))
(:typedef FlagGroup (List* (U 'once-each 'once-any 'multi)
                           (Listof FlagSpec)))
(:typedef FlagTable (Listof (U String FlagGroup)))

(define-syntax (from-command-line stx)
    (define-syntax-class string-or-strings
        #:description "string or list of strings"
        #:attributes ([strs 1])
        (pattern k:string #:with (strs ...) #'(k))
        (pattern (strs:string ...)))
    (define-syntax-class flag-spec
        #:description "specification of a flag"
        #:attributes ([result-spec 0])
        (pattern (flags:string-or-strings
                  args:id ...
                  descs:string-or-strings
                  body ...+)
                 #:with (arg-strs ...)
                        (map (lambda~> syntax->datum symbol->string (datum->syntax stx _))
                             (attribute args))
                 #:with result-spec #'(list (list flags.strs ...)
                                            (list descs.strs ...)
                                            (lambda (args ...) body ...)
                                            (list arg-strs ...))))
    (define-splicing-syntax-class flag-or-desc-group
        #:description "flag group or nonflag description"
        #:attributes ([result-spec 0])
        (pattern (~seq result-spec:str))
        (pattern (~seq (~and (~or* #:multi #:once-each #:once-any)
                             flag-group-type)
                       flag:flag-spec ...+)
                 #:with result-spec #'(list 'flag-group-type flag.result-spec ...)))
    (syntax-parse stx
        [(from-command-line
          (~alt (~optional (~seq #:cmd-name name:expr)
                           #:name "specification of command name"
                           #:defaults ([name #'"<cmd>"]))
                (~optional (~seq #:argv argv:expr)
                           #:name "specification of argument sequence"
                           #:defaults ([argv #'(current-command-line-arguments)]))
                (~optional (~seq #:help-flags (help-flags:expr ...))
                           #:name "specification of help flags"
                           #:defaults ([(help-flags 1) (list #'"--help" #'"-h" #'"-?")])))
          ...
          parts:flag-or-desc-group
          ...)
         #'(parse-command-line name argv
         ;#'(list               name argv
                               (list parts.result-spec ...)
                               (list help-flags ...))]))
                
          

(struct exn:help exn ())
(struct exn:flag:each exn (flag))
(struct exn:flag:any  exn (group))
(struct exn:flag:eof  exn ())

(: parse-command-line (String (Sequenceof String) FlagTable (Listof String) -> (Listof String)))
(define (parse-command-line cmdname argv flagtable help-flags)
    (define help-group `(#:once-each
                         (,help-flags ("Show this help.")
                          ,(lambda () (raise (exn:help "" (current-continuation-marks)))) ())))
    (define raw-table (append flagtable (list help-group)))
    (define uf-invoked (uf-new 'invoked))
    (define flag->uf (make-immutable-hash (~> raw-table
        (filter (lambda~> string? not) _)
        (map cdr _)
        (apply append _)
        (map caar _)
        (map (lambda (x) (cons x (uf-new x))) _))))
    (define ((sanitize-check exn-type) spec bad-flags)
        (match-define (list flags descs fn args) spec)
        (define (wrap . xs)
            (if* (uf-same-set? (hash-ref flag->uf (car flags)) uf-invoked)
                 (raise (exn-type "" (current-continuation-marks) bad-flags))
            (uf-union! uf-invoked (hash-ref flag->uf (car flags)))
            (apply fn xs)))
        (list flags descs wrap args))
    (define (sanitize-group group)
        (if* (string? group) group
        (match-define (cons type specs) group)
        (case type
            [(#:multi) group]
            [(#:once-each)
             (cons type (map (sanitize-check exn:flag:each) specs (map caar specs)))]
            [(#:once-any)
             (for ([spec (cdr specs)])
                (uf-union! (hash-ref flag->uf (caaar specs))
                           (hash-ref flag->uf (caar spec))))
             (cons type (map (sanitize-check exn:flag:any)
                             specs (make-list (length specs) (map caar specs))))])))
    (define table (map sanitize-group raw-table))
    (define specs (~> table
        (filter (lambda~> string? not) _)
        (map cdr _)
        (apply append _)))
    (with-handlers
        ([exn:help? (lambda _ (_help table))]
         [exn:flag:each? (lambda (e)
            (define dup (exn:flag:each-flag e))
            (raise-user-error (string->symbol cmdname)
                "multiple invocation of single-use flag ~a." dup))]
         [exn:flag:any? (lambda (e)
            (define dup (exn:flag:any-group e))
            (raise-user-error (string->symbol cmdname)
                (string-append "invocation of multiple flags in mutually-exclusive group "
                               "~a.\n  See help with ~a ~a")
                (string-join dup ", ")
                cmdname (car help-flags)))]
         [exn:flag:eof? (lambda _
            (raise-user-error (string->symbol cmdname)
                "insufficient arguments given for invoked flags.\n  See help with ~a ~a"
                cmdname (car help-flags)))])
        (_parse-argv argv specs)))

(: _find-flag (FlagLiteral (Listof FlagSpec) -> (? FlagSpec)))
(define (_find-flag flag specs)
    (if* (empty? specs) #f
;(printf "_find-flag ~s ~s\n" flag (caar specs))
    (define spec (car specs))
    (or (for/or ([flaglit (car spec)]
                 #:when (equal? flaglit flag))
            spec)
        (_find-flag flag (cdr specs)))))

(define (rcons x y) (reverse (cons x (reverse y))))
(: _parse-argv ((Sequenceof String) (Listof FlagSpec) -> (Listof String))
               ((Sequenceof String) (Listof FlagSpec) (Listof FlagSpec) (Listof String)
                -> (Listof String)))
(define (_parse-argv argv specs [awaiting-args '()] [collected-args '()])
;(pretty-write specs)
;(printf "_parse-argv ~a -- ~a ~a\n" argv awaiting-args collected-args)
    ; run effects of flags as they collect their arguments
    (if* (and (not (empty? awaiting-args))
              (<= (~> awaiting-args car cadddr length) (length collected-args)))
         (block (define argc (~> awaiting-args car cadddr length))
                (define args (~> collected-args reverse (take _ argc)))
                (define remaining (~> collected-args reverse (drop _ argc) reverse))
                (apply (~> awaiting-args car caddr) args)
                (_parse-argv argv specs (cdr awaiting-args) remaining))
    ; if no flags pending arguments, all args are to the command
    (if* (and (empty? awaiting-args) (not (empty? collected-args)))
         (append (reverse collected-args) (_parse-argv argv specs awaiting-args '()))
    (if* (empty? argv) 
         (if (empty? awaiting-args)
             (reverse collected-args)
             (raise (exn:flag:eof "" (current-continuation-marks))))
        (define flag (car argv))
        (cond
            [(equal? "--" flag)
             (_parse-argv '() specs awaiting-args (foldl cons collected-args (cdr argv)))]
            [(string-prefix? flag "--")
             (define res (_find-flag flag specs))
             (if res
                 (_parse-argv (cdr argv) specs (rcons res awaiting-args) collected-args)
                 (_parse-argv (cdr argv) specs awaiting-args (cons flag collected-args)))]
            [(and (string-prefix? flag "-") (< 2 (string-length flag)))
             (_parse-argv (append (map (lambda~> (string #\- _)) (cdr (string->list flag)))
                                  (cdr argv))
                          specs awaiting-args collected-args)]
            [(string-prefix? flag "-")
             (define res (_find-flag flag specs))
             (if res
                 (_parse-argv (cdr argv) specs (rcons res awaiting-args) collected-args)
                 (_parse-argv (cdr argv) specs awaiting-args (cons flag collected-args)))]
            [#t  (_parse-argv (cdr argv) specs awaiting-args (cons flag collected-args))])))))
                 

(: _help (FlagTable (Listof String) -> Void))
(define (_help flagtable)
    (define table (let loop ([flaggroup-or-descs flagtable])
        (if* (empty? flaggroup-or-descs) empty
        (define flaggroup-or-desc (car flaggroup-or-descs))
        (if* (string? flaggroup-or-desc)
             (cons (list "" (format "\r~a" flaggroup-or-desc))
                   (loop (cdr flaggroup-or-descs)))
        (let grouploop ([flagspecs (cdr flaggroup-or-desc)])
            (if* (empty? flagspecs)
                 (loop (cdr flaggroup-or-descs))
            (define flagspec (car flagspecs))
            (match-define `((,lit0 ,lits ...) (,desc0 ,descs ...) ,_ ,args) flagspec)
            (define topline
                (list (format "~a ~a" lit0
                              (string-join (map (lambda~> (format "<~a>" _)) args) " "))
                      desc0))
            `(,topline
              ,@(for/list ([lit  (sequence-append lits  (in-value ""))]
                            [desc (sequence-append descs (in-value ""))])
                    (list (format "    ~a" lit) desc))
              ,@(grouploop (cdr flagspecs)))))))))
    (print-table table
        '(0 0) '(100 100) '(0 2) '(left left)))
    
;(: parse-command-line (String (Sequenceof String) FlagTable (Listof String) -> (Listof String)))
;(:typedef FlagLiteral String)
;(:typedef FlagDesc String)
;(:typedef (FlagSpec a ...)
    ;(List (Listof FlagLiteral) (Listof FlagDesc) (a ... -> Void) (Listof String)))
;(:typedef FlagGroup (List* (U 'once-each 'once-any 'multi)
                           ;(Listof FlagSpec)))
;(:typedef FlagTable (Listof (U String FlagGroup)))
(module+ main
    (require racket/pretty racket/exn)
    (define flags-hit (make-hash))
    (define ((printer k) . args) (hash-set! flags-hit k args))
    (define spec `(
        (#:once-any (("-t" "--by-id")       ("order by ascending id")              ,(printer '-t) ())
                    (("-p" "--by-priority") ("order by descending priority score") ,(printer '-p) ()))
        "description line"
        (#:once-any (("-r" "--ready-only")  ("only show ready tasks")              ,(printer '-r) ())
                    (("-b" "--blocked-only") ("only show blocked tasks")           ,(printer '-b) ()))
        (#:multi    (("-a" "--assigned-to") ("only show tasks assigned to <user>"
                                             "or all <user>s, when invoked more than once") ,(printer '-a) ("user")))
        (#:once-each (("-n" "--limit")      ("limit to <n> results") ,(printer '-n) ("n")))))
    (define (tester . argv)
        (hash-clear! flags-hit)
        (define res-args (parse-command-line "list-tasks" argv spec '("-h" "--help" "-?")))
        (hash-set! flags-hit '__args res-args)
        (pretty-write (hash->list flags-hit)))

    (tester "--by-id" "--ready-only" "--assigned-to" "alice" "--limit" "20" "cpusa:")
    (tester "-t" "-r" "-a" "alice" "-n" "20" "cpusa:")
    (tester "-tra" "alice" "-n" "20" "cpusa:")
    (tester "-tran" "alice" "20" "cpusa:")
    (tester "-antr" "alice" "20" "cpusa:")
    (tester "--" "-tran")
    (tester "-t-" "-tran")
    (tester "-?")
    (define (edisplayln e) (printf "~a\n" (exn->string e)))
    (with-handlers ([exn:fail? edisplayln])
        (tester "-tran"))
    (with-handlers ([exn:fail? edisplayln])
        (tester "-tp"))

    (hash-clear! flags-hit)
    (hash-set! flags-hit '__args
        (from-command-line
            #:cmd-name "list-tasks" #:argv '("-tran" "alice" "20" "cpusa:")
            #:once-any
            [("-t" "--by-id")       "order by ascending id"
             (hash-set! flags-hit '-t '())]
            [("-p" "--by-priority") "order by descending priority score"
             (hash-set! flags-hit '-p '())]
            "description line"
            #:once-any
            [("-r" "--ready-only")  "only show ready tasks"
             (hash-set! flags-hit '-r '())]
            [("-b" "--blocked-only") "only show blocked tasks"
             (hash-set! flags-hit '-b '())]
            #:multi
            [("-a" "--assigned-to") user
             ("only show tasks assigned to <user>"
              "or all <user>s, when invoked more than once")
             (hash-update! flags-hit '-a (lambda (x) (cons user x)) '())]
            #:once-each
            [("-n" "--limit") n "limit to <n> results"
             (hash-set! flags-hit '-n (list n))]))
    (pretty-write (hash->list flags-hit))
)
        
