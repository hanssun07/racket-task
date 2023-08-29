#lang racket/base

;; homebrew implementation of racket/cmdline
;; for more expressiveness, better help

(require
    "../utils/ann.rkt"
    "../utils.rkt"
    "../repl/utils.rkt"
    racket/list
    racket/match)

(:typedef FlagLiteral String)
(:typedef FlagDesc String)
(:typedef (FlagSpec a ...) (List (Listof FlagLiteral) (Listof FlagDesc) (a ... -> Void)))
(:typedef FlagGroup (List* (U 'once-each 'once-any 'multi)
                           (Listof FlagSpec)))
(:typedef FlagTable (Listof (U String FlagGroup)))

#|
(define-syntax
|#

(: parse-command-line (String (Sequenceof String) FlagTable (Listof String) (any -> Void) -> Void))
(define (parse-command-line cmdname argv flagtable help-flags on-ready)
    (

(: _parse-argv (Sequenceof String -> Sequenceof String))

(: _help (FlagTable (Listof String) -> Void))
(define (_help flagtable help-flags)
    (define table (let loop ([flags flagtable])
        (if* (empty? flags) empty
        (match flag
            [`(,(? symbol? _) ,flags ...)
             (loop (append flags (cdr flag-groups)))]
            [`((,lit0 ,lits ...) (,desc0 ,descs ...) _)
             `((,lit0 ,desc0)
               ,@(for/list ([lit  (sequence-append lits  (in-value ""))]
                            [desc (sequence-append descs (in-value ""))])
                    (list (format "    ~a" lit) desc))
               ,@(loop (cdr flags)))]
            [(? string? desc) (cons (list "" (format "\r~a" desc)))]))))
    (print-table (append table (list (string-join help-flags " ") "Show this help."))
        '(0 0) '(100 100) '(0 2) '(left left)))
    
