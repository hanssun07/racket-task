#lang racket/base

(provide print-table)
(require
    racket/format)

(define (print-table tab min-widths max-widths gutters aligns)
    (define widths (list->vector min-widths))
    (for ([row tab])
        (for ([i (vector-length widths)] [cell row])
            (vector-set! widths i
                (min (list-ref max-widths i)
                     (max (string-length cell)
                          (vector-ref widths i))))))
    (for ([row tab])
        (for ([width (in-vector widths)]
              [cell row]
              [gutter gutters]
              [align aligns])
            (printf "~a~a"
                (make-string gutter #\space)
                (~a cell
                    #:width width
                    #:align align)))
        (newline)))
