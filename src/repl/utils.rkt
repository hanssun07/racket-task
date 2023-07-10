#lang typed/racket/base

(require
    ;(except-in type-expander let/cc for for/list)
    (only-in type-expander : define-type-expander struct define-type let lambda)
    racket/string   racket/list
        racket/function
    racket/block    racket/match
    racket/file     racket/system   racket/port
    "../types/racket-exn.rkt"
    "../domain.rkt"
    "../model.rkt"
    "../utils.rkt"
    "../utils-untyped.rkt"
    "../user.rkt"
    "utils-output.rkt")
(provide 
    CommandHandler
        CmdStr Argv Argv*
    (struct-out cmdentry)
        cmdentry-desc cmdentry-spacer
    repl-menu-switch
    retry-until-success
    prompt  prompt-multi    prompt-editor
    load    commit  commit-all  save-and-exit
    eof-barrier ?me me login
    read-line-tokens)

(define-type CmdStr (Option String))
(define-type-expander Argv
    (syntax-rules ()
        [(Argv a ...)
         (Pair CmdStr (List a ...))]))
(define-type-expander Argv*
    (syntax-rules ()
        [(Argv* a ...)
         (List* CmdStr a ...)]))

(define-type CommandHandler (Index (Argv* Any) -> Any))
(struct cmdentry
    ([matchers : (Listof CmdStr)]
     [help     : TableEntries]
     [handler  : CommandHandler])
    #:type-name CommandEntry)
(: cmdentry-desc (TableEntries -> CommandEntry))
(define (cmdentry-desc d) (cmdentry '() d void))
(define cmdentry-spacer (cmdentry-desc '(("" "" ""))))

(: repl-menu-switch (Index (Argv* Any) CmdStr (Listof CommandEntry) -> Any))
(define (repl-menu-switch argc argv cmd entries)
    (let/cc return ;: Any
        (when (or (equal? cmd "?")
                  (equal? cmd "help"))
            (define help-entries (apply append (for/list ([entry entries]) ;: (Listof TableEntries) 
                                                    (cmdentry-help entry))))
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

(define-syntax retry-until-success
    (syntax-rules (:)
        [(retry-until-success : T body ...)
         (let loop : T ()
            (let* ([failed? : Boolean #f]
                   [res : (U T Void)
                    (with-handlers
                     ([exn:break? raise]
                      [exn? (lambda ([x : exn]) (eprintf "~a" (exn->string x))
                                                (set! failed? #t))])
                     body ...)])
                  (if failed? (loop) (cast res T))))]
        [(retry-until-success body ...)
         (retry-until-success : Void body ...)]))

(: prompt (->* () ((Option String)) Void))
(: prompt-multi (-> String))
(: prompt-editor (String -> String))
(define (prompt [str #f])
    (when str (display str))
    (printf "> ")
    (flush-output))
(define (prompt-multi)
    (displayln "Empty line terminates input.")
    (prompt)
    (let loop ((lines : (Listof String) empty))
        (define ln (read-line))
        (if (or (eof-object? ln) (zero? (string-length ln)))
            (string-join (reverse lines) "\n")
            (begin (prompt)
                   (loop (cons ln lines))))))
(define (prompt-editor str) (block
    (define tmp (make-temporary-file))
    (display-to-file str tmp #:exists 'replace)
    (system (format "$EDITOR \"~a\"" tmp))
    (define res (file->string tmp))
    (delete-file tmp)
    res))

(: read-line-tokens (-> (Listof Any)))
(define (read-line-tokens [in-port (current-input-port)])
    (define ln (read-line in-port))
    (if (eof-object? ln) empty (block
    (define in (open-input-string ln "user input"))
    (define tks (port->list read in))
    (close-input-port in)
    (map (lambda ([x : Any]) (if (symbol? x) (symbol->string x) x))
         tks))))

(: load (-> Void))
(: commit (-> Void))
(: commit-all (-> Void))
(: save-and-exit (-> Nothing))
(define (load) (domain/load (assert (current-domain))))
(define (commit) (domain/commit (assert (current-domain))))
(define (commit-all)
    (for ([dmf (in-domain domain-tree-root)])
        (define dm (domain-frame-in-domain dmf))
        (when dm (domain/commit dm))))
(define (save-and-exit) (commit-all) (exit))

(: eof-barrier (All (T) (->* () ((-> T)) (U Void T))))
(: ?me (All (F) (case-> (        -> (Option User))
                        ((-> F)  -> (U User F)))))
(: me (-> User))
(define (eof-barrier [action save-and-exit]) (when (eof-object? (peek-char)) (action)))
(define ?me (case-lambda
    [()
     (define dm (assert (current-domain)))
     (define logged-in (domain-cur-user dm))
     (if* logged-in logged-in
     (login)
     (domain-cur-user dm))]
    [(on-fail) (or (?me) (on-fail))]))
(define (me) (?me (errorthunk "not logged in")))

(: login (-> Boolean))
(define (login) (let/cc return : Boolean
    (define dm (assert (current-domain)))
    (retry-until-success : Boolean (block
        (prompt "login")
        (eof-barrier (thunk (return #f)))
        (define argv (read-line-tokens))
        (define argc (length argv))
        (assert!! (= 1 argc))
        (domain/login dm (get-user-by-name (car (cast argv (List String)))))
        (printf "Logged in as ~a.\n" (car argv))
        #t))))
 

