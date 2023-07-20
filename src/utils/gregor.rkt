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

(: datum->moment (Any -> (U Moment (^ Exn:Fail))))
(define (datum->moment d)
    (adjust-timezone (iso8601->moment d) (current-timezone)))
