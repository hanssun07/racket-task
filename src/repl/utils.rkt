#lang racket/base

(require
    "../utils/ann.rkt"
    "../utils/gregor.rkt"
    threading
    racket/format   racket/string   racket/list
        racket/function     racket/exn
        racket/sequence
    racket/block    racket/match
    racket/file     racket/system   racket/port
    "../domain.rkt"
    "../model.rkt"
    "../utils.rkt")
(provide 
    (struct-out cmdentry)
        cmdentry-desc cmdentry-spacer
    repl-menu-switch
    retry-until-success
    prompt  prompt-multi    prompt-editor
    load    commit  commit-all  save-and-exit
    eof-barrier me  login
    read-token  read-line-tokens
    output-width
    format-date
    print-table)

(:typedef CmdStr (? String))
(:typedef (Argv a ...) (List* CmdStr a ...))
(:typedef Argc ExactNonnegativeInteger)
(:typedef CmdHandler (Argc Argv -> Any))
(:typedef TableRow (Listof String))
(:typedef TableEntries (Listof TableRow))

(:structdef cmdentry : CmdEntry
    ([matchers : (Listof (? String))]
     [help     : TableEntries]
     [handler  : CmdHandler]))
(: cmdentry-desc (TableEntries -> CmdEntry))
(: cmdentry-spacer CmdEntry)
(: repl-menu-switch (Argc Argv CmdStr (Listof CmdEntry) -> Any))
(struct cmdentry (matchers help handler))
(define (cmdentry-desc d) (cmdentry '() d void))
(define cmdentry-spacer (cmdentry-desc '(("" "" ""))))
(define (repl-menu-switch argc argv cmd entries)
    (let/cc return
        (when (or (equal? cmd "?")
                  (equal? cmd "help"))
            (define help-entries (apply append (for/list ([entry entries]) (cmdentry-help entry))))
            (return (print-table (append help-entries '(("" "" "") ("?" "help" "view this entry")))
                         '(0 0 0)
                         '(100 100 100)
                         '(2 2 2)
                         '(right left left))))
        (for ([entry entries])
            (match-define (cmdentry matchers _ handler) entry)
            (for ([matcher matchers])
                (when (equal? cmd matcher)
                      (return (handler argc argv)))))
        (printf "Unrecognized command ~a. (? or help for a list)\n" cmd)))

(define-syntax-rule
    (retry-until-success body ...)
    (let loop ()
        (let* ([failed? #f]
               [res (with-handlers
                        ([exn:break? raise]
                         [(const #t) (lambda (x) (eprintf "~a" (exn->string x))
                                                 (set! failed? #t))])
                        body ...)])
            (if failed? (loop) res))))

(: prompt (String -> Void) (-> Void))
(: prompt-multi (-> (Listof String)))
(: prompt-editor (String -> String))
(: read-token (-> (U String (^ Exn:Fail:Contract))))
(: read-line-tokens
    (InputPort -> Argv)
    (          -> Argv))
(define (prompt [str #f])
    (when str (display str))
    (printf "> ")
    (flush-output))
(define (prompt-multi)
    (displayln "Empty line terminates input.")
    (prompt)
    (let loop ((lines empty))
        (define ln (read-line))
        (if (zero? (string-length ln))
            (string-join (reverse lines))
            (begin (prompt)
                   (loop (cons ln lines))))))
(define (prompt-editor str)
    (define editor (or (environment-variables-ref (current-environment-variables) #"EDITOR")
                       (environment-variables-ref (current-environment-variables) #"VISUAL")))
    (if** editor
        ((define tmp (make-temporary-file))
         (display-to-file str tmp #:exists 'replace)
         (system (format "~a \"~a\"" editor tmp))
         (define res (file->string tmp))
         (delete-file tmp)
         res)
        (printf "Overwriting the following:\n")
        (displayln str)
        (newline)
        (string-join (prompt-multi) "\n")))
(define (read-token) (symbol->string (read)))
(define (read-line-tokens [in-port (current-input-port)])
    (define ln (read-line in-port))
    (define in (open-input-string ln "user input"))
    (define tks (port->list read in))
    (close-input-port in)
    (map (lambda (x) (if (symbol? x) (symbol->string x) x))
         tks))

(: load (-> Void))
(: commit (-> Void))
(: commit-all (-> Void))
(: save-and-exit (-> Never))
(define (load) (domain/load (current-domain)))
(define (commit) (domain/commit (current-domain)))
(define (commit-all)
    (for ([dmf (in-domain domain-tree-root)])
        (domain/commit (domain-frame-in-domain dmf))))
(define (save-and-exit) (commit-all) (exit))

(: eof-barrier
    ((-> t) -> (U Void t))
    (       -> Void))
(: me (-> (? User)))
(: login (-> Bool))
(define (eof-barrier [action save-and-exit]) (when (eof-object? (peek-char)) (action)))
(define (me)
    (define logged-in (domain-cur-user (current-domain)))
    (or logged-in
        (and (login)
             (domain-cur-user (current-domain)))))
(define (login) (let/cc return
    (retry-until-success (block
        (prompt "login")
        (eof-barrier (thunk* (return #f)))
        (define argv (read-line-tokens))
        (define argc (length argv))
        (assert!! (= 1 argc))
        (domain/login (current-domain) (get-user-by-name (car argv)))
        (printf "Logged in as ~a.\n" (car argv))
        #t))))

(: string-empty? (String -> Bool))
(: string-display-control-prefix (String -> String))
(define (string-empty? str) (zero? (string-length str)))
(define (string-display-control-prefix str)
    (if (string-prefix? str "\033[")
        (let ([is (regexp-match-positions "m" str)])
            (if is (substring str 0 (add1 (caar is))) ""))
        ""))

(: string-length/displayed (String -> Index))
(: string-truncate/displayed (String Index -> String))
(define (string-length/displayed str)
    (string-length (regexp-replace* #rx"\033\\[[0-9;]+m" str "")))
(define (string-truncate/displayed str len)
    (define parts (reverse (let loop ([rest str] [left len] [acc '()])
        (if* (string-empty? rest) acc
        (define pre (string-display-control-prefix rest))
        (if (string-empty? pre)
            (if (zero? left)
                (loop (substring rest 1) 0 acc)
                (loop (substring rest 1) (sub1 left) (cons (string-ref rest 0) acc)))
            (loop (substring rest (string-length pre))
                  left (cons pre acc)))))))
    (foldr (lambda (x str) (string-append (if (string? x) x (make-string 1 x)) str)) 
           "" parts))

(: output-width ((-> Any) -> Index))
(define (output-width th)
    (define str (open-output-string))
    (parameterize ([current-output-port str]) (th))
    (~> (get-output-string str)
        (string-split _ "\n" #:trim? #f #:repeat? #t)
        (argmax string-length/displayed _)
        string-length/displayed))
 
(: format-date (Moment -> String))
(: print-table (TableEntries (Listof ExactNonnegativeInteger) x4 -> Void))
(define (format-date dt)
    (~t dt "y-MM-dd"))
(define (print-table tab min-widths max-widths gutters aligns
            #:ncols [ncols (length min-widths)]
            #:elide-repeated? [elide-repeated?s (make-list ncols #f)])
    (define widths (list->vector min-widths))
    (for ([row tab])
        (define spill?s (for/foldr ([res '(#f)] [cur #t] #:result res)
                                   ([cell (cdr row)])
                                   (define spill? (and cur (string-empty? cell)))
                                   (values (cons spill? res) spill?)))
        (for ([i (vector-length widths)] [cell row] [spill? spill?s])
            (unless spill?
                (vector-set! widths i
                    (min (list-ref max-widths i)
                         (max (string-length/displayed cell)
                              (vector-ref widths i)))))))
    (define lasts (make-list ncols ""))
    (for ([row tab])
        (define spaces+spillover
            (for/foldr ([res (list (car (reverse (vector->list widths))))]
                        [spill? #t]
                        #:result res)
                       ([width (in-vector widths)]
                        [ngutter (cdr gutters)]
                        [ncell   (cdr row)]
                        [cell    row])
                (values 
                    (if spill?  
                        (cons (+ width (if (string-empty? ncell) (+ (car res) ngutter) 0))
                              res)
                        (cons width res))
                    (and spill? (string-empty? cell)))))
        (for/fold
             ([missing-space (car gutters)])
             ([cwidth (in-vector widths)]
              [width spaces+spillover]
              [cell row]
              [ngutter (sequence-append (cdr gutters) (in-value 0))]
              [align aligns]
              [elide-repeated? elide-repeated?s]
              [last lasts])
            (define cell-str (if (eq? align 'left)
                (~a (if (and elide-repeated? (equal? cell last))
                        "" (string-truncate/displayed cell width)))
                (~a (if (and elide-repeated? (equal? cell last))
                        "" (string-truncate/displayed cell width))
                    #:width (+ width (- (string-length cell) (string-length/displayed cell)))
                    #:align align)))
            (printf "~a~a"
                (make-string (max 0 missing-space) #\space)
                cell-str)
            (+ missing-space ngutter (- cwidth (string-length/displayed cell-str))))
        (set! lasts row)
        (newline)))

