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
))

(:typedef FlagLiteral String)
(:typedef FlagDesc String)
(:typedef (FlagSpec a ...)
    (List (Listof FlagLiteral) (Listof FlagDesc) (a ... -> Void) (Listof String)))
(:typedef FlagGroup (List* (U 'once-each 'once-any 'multi)
                           (Listof FlagSpec)))
(:typedef FlagTable (Listof (U String FlagGroup)))

#|
(define-syntax
|#
(struct exn:help exn ())
(struct exn:flag:each exn (flag))
(struct exn:flag:any  exn (group))
(struct exn:flag:eof  exn ())

(: parse-command-line (String (Sequenceof String) FlagTable (Listof String) -> (Listof String)))
(define (parse-command-line cmdname argv flagtable help-flags)
    (define help-group `(once-each
                         (,help-flags "Show this help." ,(lambda () (raise (exn:help))) ())))
    (define raw-table (append flagtable (list help-group)))
    (define uf-invoked (uf-new 'invoked))
    (define flag->uf (make-immutable-hash (~> raw-table
        (filter (lambda~> string? not) _)
        (map cdr _)
        (apply append _)
        (map caar _)
        (map (lambda (x) (cons x (uf-new x)))))))
    (define ((sanitize-check exn-type) spec bad-flags)
        (match-define (list flags descs fn args) spec)
        (define (wrap . xs)
            (if* (uf-same-set? (hash-ref flag->uf (car flags)) uf-invoked)
                 (raise (exn-type bad-flags))
            (uf-union! uf-invoked (hash-ref flag->uf (car flags)))
            (apply fn xs)))
        (list flags descs wrap args))
    (define (sanitize-group group)
        (if* (string? group) group
        (match-define (cons type specs) group)
        (case type
            [(multi) group]
            [(once-each)
             (cons type (map (sanitize-check exn:flag:each) specs (map caar specs)))]
            [(once-any)
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
    (define spec (car specs))
    (for/or ([flaglit (car spec)]
             #:when (equal? flaglit flag))
        spec)))

(: _parse-argv ((Sequenceof String) (Listof FlagSpec) -> (Listof String))
               ((Sequenceof String) (Listof FlagSpec) (Listof FlagSpec) (Listof String)
                -> (Listof String)))
(define (_parse-argv argv specs [awaiting-args '()] [collected-args '()])
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
             (raise (exn:flag:eof)))
        (define flag (car argv))
        (cond
            [(equal? "--" flag)
             (_parse-argv '() specs awaiting-args (foldl cons collected-args (cdr argv)))]
            [(string-prefix? flag "--")
             (define res (_find-flag flag specs))
             (if res
                 (_parse-argv (cdr argv) specs (cons res awaiting-args) collected-args)
                 (_parse-argv (cdr argv) specs awaiting-args (cons flag (collected-args))))]
            [(and (string-prefix? flag "-") (< 2 (string-length flag)))
             (_parse-argv (append (map (lambda~> (string #\- _)) (cdr (string->list flag)))
                                  (cdr argv))
                          specs awaiting-args collected-args)]
            [(string-prefix? flag "-")
             (define res (_find-flag flag specs))
             (if res
                 (_parse-argv (cdr argv) specs (cons res awaiting-args) collected-args)
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
            (match-define `((,lit0 ,lits ...) (,desc0 ,descs ...) _ ,args) flagspec)
            (define topline
                (list (format "~a ~a" lit0
                              (string-join (map (lambda~> (format "<~a>" _)) args) " "))
                      desc0))
            `((,topline
               ,@(for/list ([lit  (sequence-append lits  (in-value ""))]
                            [desc (sequence-append descs (in-value ""))])
                    (list (format "    ~a" lit) desc))
               ,@(grouploop (cdr flagspecs))))))))))
    (print-table table
        '(0 0) '(100 100) '(0 2) '(left left)))
    
