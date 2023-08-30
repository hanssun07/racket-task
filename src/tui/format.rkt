#lang racket/base

(require
    "../utils/ann.rkt"
    "../utils.rkt"
    threading
    racket/string racket/list racket/sequence
    racket/format
    racket/port
    ;syntax/parse
    (for-syntax
        racket/base syntax/parse racket/syntax
    )
)

(: _print-string/display (DisplayString OutputPort (U Bool 0 1) -> Void))
(define (_print-string/display sd port mode)
    (if* mode (write-string "#<string/display>" port)
    (write-string (string/display->string sd) port)))

(:structdef tui-fmt : TuiFmt ([code : String]))
(struct tui-fmt (code)
    #:methods gen:custom-write
    [(define write-proc _print-string/display)])

(define (tui-fmt-combine . tui-fmts)
    (~> (map tui-fmt-code tui-fmts)
        (string-join _ ";")
        (tui-fmt _)))

(:structdef _string/display : String/Display ([elems : (Listof (U TuiFmt String))]))
(struct _string/display (elems) #:transparent
    #:methods gen:custom-write
    [(define write-proc _print-string/display)])

(: s/d-cons ((U TuiFmt String) DisplayString -> DisplayString))
(: s/d-car  (DisplayString -> (U TuiFmt String (^ Exn:Fail))))
(: s/d-cdr  (DisplayString -> (U DisplayString (^ Exn:Fail))))
(: s/d-empty DisplayString)
(: s/d-empty? DisplayString -> Bool)
(define (s/d-cons a b)  (~> (_string/display-elems b) (cons a _) (_string/display _)))
(define (s/d-car  sd)   (~> (_string/display-elems sd) (car _)))
(define (s/d-cdr  sd)   (~> (_string/display-elems sd) (cdr _) (_string/display _)))
(define s/d-empty       (_string/display '()))
(define (s/d-empty? sd) (~> (_string/display-elems sd) (empty? _)))

(: s/d (Any * -> DisplayString))
(define (s/d . elems)
    (define (->string/display a) (cond
        [(string?          a) a]
        [(tui-fmt?         a) a]
        [(_string/display? a) a]
        [#t                   (~a a)]))
    (define (merge-string/display e es)
        (if (_string/display? e)
            (append (_string/display-elems e) es)
            (cons e es)))
    (~> (map ->string/display elems)
        (foldr merge-string/display '() _)
        _string/display))

(:typedef DisplayString (U String TuiFmt String/Display))

(: tui-fmt->string (TuiFmt -> String))
(define (tui-fmt->string t) (format "\033[~am" (tui-fmt-code t)))

(: string/display->string (DisplayString Bool -> String))
(define (string/display->string s [format? #t]) (cond
    [(string?          s) s]
    [(tui-fmt?         s) (if format? (tui-fmt->string s) "")]
    [(_string/display? s)
     (~> (_string/display-elems s)
         (map (lambda~> (string/display->string _ format?)) _)
         (apply string-append _))]))

(: string/display-length    (DisplayString -> Index))
(: string/display-substr    (DisplayString Index Index -> DisplayString))
(: string/display-truncate  (DisplayString Index (U 'left 'right 'center) -> DisplayString))
(: string/display-search*   (DisplayString StrPattern 
                             (#:match-select ((Listof (Pair Index Index)) -> a))
                             -> (Listof a))
                            #:def (a (Pair Index Index)))
(: string/display-reflow    (DisplayString Index -> (Listof DisplayString)))
(: string/display-dims      (DisplayString Index -> (Pair Index Index)))
(: string/display-width     (DisplayString Index -> Index))
(: string/display-height    (DisplayString Index -> Index))

(define (string/display-length s)
    (~> (s/d s)
        (_string/display-elems _)
        (filter string? _)
        (map string-length _)
        (foldl + 0 _)))

(define (string/display-substr s start end)
    (define elems (~> (s/d s) (_string/display-elems _)))
    (define res (let loop ([i 0] [elems elems])
        (if* (empty? elems) empty
        (define next (car elems))
        (if* (tui-fmt? next) (cons next (loop i (cdr elems)))
        (define next-len (string-length next))
        (define this-value (cond
            [(<= (+ i next-len) start)       #f]
            [(<  end i)                      #f]
            [(<= start i (+ i next-len) end) next]
            [#t (substring next (max 0 (- start i)) (min next-len (- end i)))]))
        (if this-value
            (cons this-value (loop (+ i next-len) (cdr elems)))
            (loop (+ i next-len) (cdr elems)))))))
    (_string/display res))

(define (string/display-truncate s len [align 'left])
    (if* (not (_string/display? s)) (string/display-truncate (s/d s) len align)
    (define len-s (string/display-length s))
    (if* (<= len-s len) s
    (define start (case align
        [(left) 0] [(right) (- len-s len)] [(center) (quotient (- len-s len) 2)]))
    (string/display-substr s start (+ start len)))))

(define (string/display-search* s pat
         #:match-select [match-select car])
    (define dstr (string/display->string s #f))
    (regexp-match-positions* pat dstr #:match-select match-select))

(define (string/display-reflow s width)
    (if* (not (_string/display? s)) (string/display-reflow (s/d s) width)
    (define newline-idxs (string/display-search* s "\n" #:match-select caar))
    (define space-idxs (string/display-search* s " " #:match-select caar))
    (define len (string/display-length s))
    (define (gather-breaks ns ss [i 0])
        (define ni (if (empty? ns) +inf.0 (car ns)))
        (define si (if (empty? ss) +inf.0 (car ss)))
        (define si+ (if (empty? ss) #f (if (empty? (cdr ss)) len (cadr ss))))
        (if* (<= si i)                      (gather-breaks ns (cdr ss) i)
        (if* (<= ni (+ i width))            (cons (list ni 1) (gather-breaks (cdr ns) ss (add1 ni)))
        (if* (<= len (+ i width))           (list (list len 0))
        (if* (and si+ (= (add1 si) si+))    (gather-breaks ns (cdr ss) i)
        (if* (and si+ (< (+ i width) si+))  (cons (list si 1) (gather-breaks ns (cdr ss) (add1 si)))
        (if* (< (+ i width) si)             (cons (list (+ i width) 0)
                                                  (gather-breaks ns ss (+ i width)))
        (gather-breaks ns (cdr ss) i))))))))
    (define breaks (gather-breaks newline-idxs space-idxs))
    (define starts (cons 0 (map + (map car breaks) (map cadr breaks))))
    (define ends (map car breaks))
    (for/list ([start starts] [end ends])
        (string/display-substr s start end))))

(define (string/display-dims s [maxwidth +inf.0])
    (define reflowed (string/display-reflow s maxwidth))
    (cons (length reflowed)
          (argmax values (map string/display-length reflowed))))
(define (string/display-width s [maxwidth +inf.0])
    (cdr (string/display-dims s maxwidth)))
(define (string/display-height s [maxwidth +inf.0])
    (car (string/display-dims s maxwidth)))

(define-syntax (define-tui-fmt stx)
    (syntax-parse stx
        [(form name:id code:string)
         (with-syntax ([fmtcode-id (format-id #'form "fmtcode-~a" #'name)]
                       [tui-obj-id (format-id #'form "tui-~a" #'name)])
             #'(define-values (fmtcode-id tui-obj-id)
                 (values code (tui-fmt code))))]))
(define-tui-fmt reset         "0")
(define-tui-fmt bold          "1")
(define-tui-fmt under         "4")
(define-tui-fmt fore-black    "38;5;0")
(define-tui-fmt fore-red      "38;5;1")
(define-tui-fmt fore-green    "38;5;2")
(define-tui-fmt fore-yellow   "38;5;3")
(define-tui-fmt fore-blue     "38;5;4")
(define-tui-fmt fore-magenta  "38;5;5")
(define-tui-fmt fore-cyan     "38;5;6")
(define-tui-fmt fore-white    "38;5;7")
(define-tui-fmt fore+black    "38;5;8")
(define-tui-fmt fore+red      "38;5;9")
(define-tui-fmt fore+green    "38;5;10")
(define-tui-fmt fore+yellow   "38;5;11")
(define-tui-fmt fore+blue     "38;5;12")
(define-tui-fmt fore+magenta  "38;5;13")
(define-tui-fmt fore+cyan     "38;5;14")
(define-tui-fmt fore+white    "38;5;15")
(define-tui-fmt back-black    "48;5;0")
(define-tui-fmt back-red      "48;5;1")
(define-tui-fmt back-green    "48;5;2")
(define-tui-fmt back-yellow   "48;5;3")
(define-tui-fmt back-blue     "48;5;4")
(define-tui-fmt back-magenta  "48;5;5")
(define-tui-fmt back-cyan     "48;5;6")
(define-tui-fmt back-white    "48;5;7")
(define-tui-fmt back+black    "48;5;8")
(define-tui-fmt back+red      "48;5;9")
(define-tui-fmt back+green    "48;5;10")
(define-tui-fmt back+yellow   "48;5;11")
(define-tui-fmt back+blue     "48;5;12")
(define-tui-fmt back+magenta  "48;5;13")
(define-tui-fmt back+cyan     "48;5;14")
(define-tui-fmt back+white    "48;5;15")

(module+ main
    (define displaystr (s/d
        "hello\n"
        tui-bold
        "1 2 3 4 5 6 7 8 9 0 "
        tui-reset tui-under
        "11 22 33 44 55 66 77 88 99 00 "
        tui-bold
        "111 222 333 444 555 666 777 888 999 000 "
        tui-reset tui-fore-blue tui-back-white
        "1111    2222    3333    4444    5555    "
        tui-reset
        "6666    7777    8888    9999    0000    "
        tui-back+black
        "\nnewline\nnewline                                     "
        "longlineqwertyuiopasdfghjklzxcvbnm"
        tui-reset
    ))
    (for ([sd (string/display-reflow displaystr 20)])
        (displayln sd)))
