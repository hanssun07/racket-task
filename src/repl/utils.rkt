#lang racket/base

(require
    racket/string   racket/list
        racket/function     racket/exn
    racket/block    racket/match
    racket/file     racket/system   racket/port
    "../domain.rkt"
    "../model.rkt"
    "../utils.rkt"
    "../utils-untyped.rkt"
    "utils-output.rkt")
(provide 
    (struct-out cmdentry)
        cmdentry-desc cmdentry-spacer
    repl-menu-switch
    retry-until-success
    prompt  prompt-multi    prompt-editor
    load    commit  commit-all  save-and-exit
    eof-barrier me  login
    read-token  read-line-tokens)

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

(define (load) (domain/load (current-domain)))
(define (commit) (domain/commit (current-domain)))
(define (commit-all)
    (for ([dmf (in-domain domain-tree-root)])
        (domain/commit (domain-frame-in-domain dmf))))
(define (save-and-exit) (commit-all) (exit))

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
 

