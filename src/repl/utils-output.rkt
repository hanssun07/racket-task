#lang typed/racket/base

(require
    "../types.rkt"
    racket/date    racket/list
    racket/format  racket/match)
(provide
    format-date
    print-table)

(: format-date : Timestamp -> String)
(define (format-date dt)
    (: ~00 : Timestamp -> String)
    (define (~00 x) (~r x #:min-width 2 #:pad-string "0"))
    (match-define (date _ _ _ d m y _ _ _ _) (seconds->date dt))
    (format "~a-~a-~a" y (~00 m) (~00 d)))

(: print-table (->* ((Listof (Listof String))
                     (Listof Index)
                     (Listof Index)
                     (Listof Index)
                     (Listof (U 'center 'left 'right)))
                    (#:ncols Index
                     #:elide-repeated? (Listof Boolean))
                    Void))
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
              [align : (U 'center 'left 'right) aligns]
              [elide-repeated? elide-repeated?s]
              [last lasts]
              #:when (positive? width))
            (printf "~a~a"
                (make-string gutter #\space)
                (~a (if (and elide-repeated? (equal? cell last)) "" cell)
                    #:width width
                    #:align align)))
        (set! lasts row)
        (newline)))


