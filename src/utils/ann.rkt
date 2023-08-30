#lang racket/base

(provide
    : :typedef :structdef)

(define type-registry (make-hash))
(define (register-type name type)
    (printf "registering ~a for ~a " name type)
    (hash-update! type-registry name (lambda (ts) (cons type ts)) '())
    (printf " (~a entries)\n" (length (hash-ref type-registry name))))

(module+ reader
    (provide type-registry))

(require
    (for-syntax racket/base syntax/parse))

(begin-for-syntax
    (define doc? (environment-variables-ref (current-environment-variables) #"DOC"))
    (define (doc-or-noop stx)
        (if doc? stx #'(begin))))

;; usage
#;(require (submod "src/utils/ann.rkt" reader))
#;(for ([(n ts) (in-hash type-registry)])
    (displayln n)
    (for-each displayln ts)
    (newline))

(define-syntax-rule
    (:typedef cons type)
    (begin))
(define-syntax :structdef
    (syntax-rules (:)
        [(_ name : tname ([fname : ftype] ...))
         (begin)]))

(define-syntax (: stx)
    (syntax-parse stx
        [(_ name:id types ...)
         (doc-or-noop
          #'(for ([type (in-list (list 'types ...))])
             (register-type 'name type)))]))

