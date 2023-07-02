#lang racket

(require racket/date racket/block)

(define args (current-command-line-arguments))
(define watchdir (vector-ref args 0))
(define cmdrun (vector-ref args 1))

(define (build-change-event dir)
    ;(define dir-event (wrap-evt (filesystem-change-evt dir) (const dir)))
    (define in-dir (map path->string (directory-list dir)))
    (define rel-in-dir (filter (lambda (x) (not (string-prefix? x "."))) in-dir))
    (define paths-in-dir (map (lambda (x) (string-append dir "/" x)) rel-in-dir))
    (define files-in (filter (lambda (x) (equal? 'file (file-or-directory-type x))) paths-in-dir))
    (define dirs-in (filter (lambda (x) (equal? 'directory (file-or-directory-type x))) paths-in-dir))
    (define dir-events (map build-change-event dirs-in))
    (define file-events (map (lambda (x)
        (wrap-evt (filesystem-change-evt x) (const x))) files-in))
    (apply choice-evt
        ;dir-event
        (append dir-events file-events)))

(define (run)
    (time (system cmdrun))
    (block
        (define changed (sync/enable-break 
            (choice-evt
                (wrap-evt (read-bytes!-evt (make-bytes 1) (current-input-port))
                    (const "[requested]"))
                (build-change-event watchdir))))
        (printf "\n\nChanged: ~a\n" changed)
        (match-define (date s m h _ _ _ _ _ _ _) (current-date))
        (define (~t k) (~r k #:min-width 2 #:pad-string "0"))
        (printf "\nRunning at ~a:~a:~a...\n" (~t h) (~t m) (~t s))
        (printf "  $ ~a\n\n" cmdrun))
    (collect-garbage 'major)
    (run))
    
(block
    (match-define (date s m h _ _ _ _ _ _ _) (current-date))
    (define (~t k) (~r k #:min-width 2 #:pad-string "0"))
    (printf "\n\nRunning at ~a:~a:~a...\n" (~t h) (~t m) (~t s))
    (printf "  $ ~a\n\n" cmdrun))
(run) 
