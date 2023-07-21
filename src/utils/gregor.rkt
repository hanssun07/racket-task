#lang racket/base

(require
    gregor
    "ann.rkt")
(provide (all-from-out gregor)
    moment->datum
    datum->moment)

(: moment->datum (Moment -> Any))
(define (moment->datum m)
    (moment->iso8601 (adjust-timezone m 0)))

(: datum->moment (Any (e -> f) -> (U Moment f))
                 (Any f        -> (U Moment f))
                 (Any          -> (U Moment (^ Exn:Fail))))
(define (datum->moment d [on-fail #f])
    (with-handlers
        ([exn:fail:contract? (lambda (e) (if (procedure? on-fail) (on-fail e) on-fail))])
        (adjust-timezone (iso8601->moment d) (current-timezone))))
