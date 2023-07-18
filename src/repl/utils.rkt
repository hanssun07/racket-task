#lang racket/base

(require
    "../utils/ann.rkt"
    racket/format   racket/string   racket/list
        racket/function     racket/exn
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
(define (prompt-editor str) (block
    (define tmp (make-temporary-file))
    (display-to-file str tmp #:exists 'replace)
    (system (format "$EDITOR \"~a\"" tmp))
    (define res (file->string tmp))
    (delete-file tmp)
    res))
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
 
(: format-date (Date -> String))
(: print-table (TableEntries (Listof ExactNonnegativeIntegers) x4 -> Void))
(define (format-date dt)
    (define (~00 x) (~r x #:min-width 2 #:pad-string "0"))
    (match-define (date _ _ _ d m y _ _ _ _) (seconds->date dt))
    (format "~a-~a-~a" y (~00 m) (~00 d)))
(define (print-table tab min-widths max-widths gutters aligns
            #:ncols [ncols (length min-widths)]
            #:elide-repeated? [elide-repeated?s (make-list ncols #f)])
    (define widths (list->vector min-widths))
    (for ([row tab])
        (for ([i (vector-length widths)] [cell row])
            (vector-set! widths i
                (min (list-ref max-widths i)
                     (max (string-length cell)
                          (vector-ref widths i))))))
    (define lasts (make-list ncols ""))
    (for ([row tab])
        (for ([width (in-vector widths)]
              [cell row]
              [gutter gutters]
              [align aligns]
              [elide-repeated? elide-repeated?s]
              [last lasts])
            (printf "~a~a"
                (make-string gutter #\space)
                (~a (if (and elide-repeated? (equal? cell last)) "" cell)
                    #:width width
                    #:align align)))
        (set! lasts row)
        (newline)))


