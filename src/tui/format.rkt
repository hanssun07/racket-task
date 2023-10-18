#lang racket/base

(require
    "../utils/ann.rkt"
    "../utils.rkt"
    threading
    racket/string racket/list racket/sequence
    racket/format
    racket/match racket/block
    racket/port
    ;syntax/parse
    (for-syntax
        racket/base syntax/parse racket/syntax
    )
)

(: _print-tui:string (TuiString OutputPort (U Bool 0 1) -> Void))
(define (_print-tui:string sd port mode)
    (if* mode (write-string "#<tui:string>" port)
    (write-string (tui:string->string sd) port)))

(:typedef FmtSpec (U (listof String) 'cancel))
(:structdef tui:fmt : TuiFmt
    ([bold      : FmtSpec]
     [under     : FmtSpec]
     [fore      : FmtSpec]
     [back      : FmtSpec]))
(struct tui:fmt (bold? under? fore back)
    #:methods gen:custom-write
    [(define write-proc _print-tui:string)])

(: tui:fmt-combine ((Listof TuiFmt) -> TuiFmt))
(define tui:fmt-combine (block
    (define (fmtspec-join a b) (match* (a b)
        [((cons a as) 'clear)  as]
        [((? list?) (? list?)) (append b a)]))
    (define (tui:fmt-merge a b)
        (match-define (tui:fmt ab au af ak) a)
        (match-define (tui:fmt bb bu bf bk) b)
        (list (tui:fmt (fmtspec-join ab bb)
                       (fmtspec-join au bu)
                       (fmtspec-join af bf)
                       (fmtspec-join ak bk))))
    (lambda fmts (car (list-inner-merge tui:fmt-merge fmts)))))

(: tui:fmt->string (TuiFmt -> String))
(define (tui:fmt->string fmt)
    (define (fmtspec->str? f) (and (pair? f) (car f)))
    (match-define (tui:fmt b u f k) fmt)
    (~> (list b u f k)
        (map fmtspec->str? _)
        (filter values _)
        (cons "0" _) ; reset code
        (string-join _ ";")
        (format "\033[~am" _)))

(:structdef _tui:string : TuiStringGroup ([elems : (Listof (U TuiFmt String))]))
(struct _tui:string (elems) #:transparent
    #:methods gen:custom-write
    [(define write-proc _print-tui:string)])

(: tui:string-cons ((U TuiFmt String) TuiString -> TuiString))
(: tui:string-car  (TuiString -> (U TuiFmt String (^ Exn:Fail))))
(: tui:string-cdr  (TuiString -> (U TuiString (^ Exn:Fail))))
(: tui:string-empty TuiString)
(: tui:string-empty? TuiString -> Bool)
(define (tui:string-cons a b)  (~> (_tui:string-elems b) (cons a _) (_tui:string _)))
(define (tui:string-car  sd)   (~> (_tui:string-elems sd) (car _)))
(define (tui:string-cdr  sd)   (~> (_tui:string-elems sd) (cdr _) (_tui:string _)))
(define tui:string-empty       (_tui:string '()))
(define (tui:string-empty? sd) (~> (_tui:string-elems sd) (empty? _)))

(: tui:string (Any * -> TuiString))
(define (tui:string . elems)
    (define (->tui:string a) (cond
        [(string?          a) a]
        [(tui:fmt?         a) a]
        [(_tui:string? a) a]
        [#t                   (~a a)]))
    (define (merge-tui:string e es)
        (if (_tui:string? e)
            (append (_tui:string-elems e) es)
            (cons e es)))
    (~> (map ->tui:string elems)
        (foldr merge-tui:string '() _)
        _tui:string))

(:typedef TuiString (U String TuiFmt TuiStringGroup))

(: tui:string->string (TuiString Bool -> String))
(define (tui:string->string s [format? #t])
    (: tui:string-flatten (TuiString -> (listof (U String TuiFmt))))
    (define (tui:string-flatten s) (match s
        [(? string?)  (list s)]
        [(? tui:fmt?) (list s)]
        [(_tui:string elems) (apply append (map tui:string-flatten elems))]))
    (define flat (tui:string-flatten s))
    (define strs (let loop ([elems flat] [ctx tui:fmt:identity]) (match elems
        [(cons (? string?  s) es) (cons s (loop es ctx))]
        [(cons (? tui:fmt? f) es)
         (define newfmt (tui:fmt-combine ctx f))
         (if format?
             (cons (tui:fmt->string newfmt) (loop es newfmt))
             (loop es newfmt))]   
        ['() '()])))
    (apply string-append strs))

(: tui:string-length    (TuiString -> Index))
(: tui:string-substr    (TuiString Index Index -> TuiString))
(: tui:string-truncate  (TuiString Index (U 'left 'right 'center) -> TuiString))
(: tui:string-search*   (TuiString StrPattern 
                             (#:match-select ((Listof (Pair Index Index)) -> a))
                             -> (Listof a))
                            #:def (a (Pair Index Index)))
(: tui:string-reflow    (TuiString Index -> (Listof TuiString)))
(: tui:string-dims      (TuiString Index -> (Pair Index Index)))
(: tui:string-width     (TuiString Index -> Index))
(: tui:string-height    (TuiString Index -> Index))

(define (tui:string-length s)
    (~> (tui:string s)
        (_tui:string-elems _)
        (filter string? _)
        (map string-length _)
        (foldl + 0 _)))

(define (tui:string-substr s start end)
    (define elems (~> (tui:string s) (_tui:string-elems _)))
    (define res (let loop ([i 0] [elems elems])
        (if* (empty? elems) empty
        (define next (car elems))
        (if* (tui:fmt? next) (cons next (loop i (cdr elems)))
        (define next-len (string-length next))
        (define this-value (cond
            [(<= (+ i next-len) start)       #f]
            [(<  end i)                      #f]
            [(<= start i (+ i next-len) end) next]
            [#t (substring next (max 0 (- start i)) (min next-len (- end i)))]))
        (if this-value
            (cons this-value (loop (+ i next-len) (cdr elems)))
            (loop (+ i next-len) (cdr elems)))))))
    (_tui:string res))

(define (tui:string-truncate s len [align 'left])
    (if* (not (_tui:string? s)) (tui:string-truncate (tui:string s) len align)
    (define len-s (tui:string-length s))
    (if* (<= len-s len) s
    (define start (case align
        [(left) 0] [(right) (- len-s len)] [(center) (quotient (- len-s len) 2)]))
    (tui:string-substr s start (+ start len)))))

(define (tui:string-search* s pat
         #:match-select [match-select car])
    (define dstr (tui:string->string s #f))
    (regexp-match-positions* pat dstr #:match-select match-select))

(define (tui:string-reflow s width)
    (if* (not (_tui:string? s)) (tui:string-reflow (tui:string s) width)
    (define newline-idxs (tui:string-search* s "\n" #:match-select caar))
    (define space-idxs (tui:string-search* s " " #:match-select caar))
    (define len (tui:string-length s))
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
        (tui:string-substr s start end))))

(define (tui:string-dims s [maxwidth +inf.0])
    (define reflowed (tui:string-reflow s maxwidth))
    (cons (length reflowed)
          (argmax values (map tui:string-length reflowed))))
(define (tui:string-width s [maxwidth +inf.0])
    (cdr (tui:string-dims s maxwidth)))
(define (tui:string-height s [maxwidth +inf.0])
    (car (tui:string-dims s maxwidth)))

(define (make-tui:fmt b u f k)
    (define (s->f x) (match x
        [(? string?) (list x)]
        [#f          (list)]
        ['clear      x]))
    (tui:fmt (s->f b) (s->f u) (s->f f) (s->f k)))
(define tui:fmt:identity     (make-tui:fmt #f     #f #f        #f))
(define tui:fmt:bold         (make-tui:fmt "1"    #f #f        #f))
(define tui:fmt:unbold       (make-tui:fmt 'clear #f #f        #f))
(define tui:fmt:under        (make-tui:fmt #f    "4" #f        #f))
(define tui:fmt:ununder      (make-tui:fmt #f 'clear #f        #f))
(define tui:fmt:fore-black   (make-tui:fmt #f     #f "38;5;0"  #f))
(define tui:fmt:fore-red     (make-tui:fmt #f     #f "38;5;1"  #f))
(define tui:fmt:fore-green   (make-tui:fmt #f     #f "38;5;2"  #f))
(define tui:fmt:fore-yellow  (make-tui:fmt #f     #f "38;5;3"  #f))
(define tui:fmt:fore-blue    (make-tui:fmt #f     #f "38;5;4"  #f))
(define tui:fmt:fore-magenta (make-tui:fmt #f     #f "38;5;5"  #f))
(define tui:fmt:fore-cyan    (make-tui:fmt #f     #f "38;5;6"  #f))
(define tui:fmt:fore-white   (make-tui:fmt #f     #f "38;5;7"  #f))
(define tui:fmt:fore+black   (make-tui:fmt #f     #f "38;5;8"  #f))
(define tui:fmt:fore+red     (make-tui:fmt #f     #f "38;5;9"  #f))
(define tui:fmt:fore+green   (make-tui:fmt #f     #f "38;5;10" #f))
(define tui:fmt:fore+yellow  (make-tui:fmt #f     #f "38;5;11" #f))
(define tui:fmt:fore+blue    (make-tui:fmt #f     #f "38;5;12" #f))
(define tui:fmt:fore+magenta (make-tui:fmt #f     #f "38;5;13" #f))
(define tui:fmt:fore+cyan    (make-tui:fmt #f     #f "38;5;14" #f))
(define tui:fmt:fore+white   (make-tui:fmt #f     #f "38;5;15" #f))
(define tui:fmt:unfore       (make-tui:fmt #f     #f 'clear    #f))
(define tui:fmt:back-black   (make-tui:fmt #f     #f #f  "48;5;0"))
(define tui:fmt:back-red     (make-tui:fmt #f     #f #f  "48;5;1"))
(define tui:fmt:back-green   (make-tui:fmt #f     #f #f  "48;5;2"))
(define tui:fmt:back-yellow  (make-tui:fmt #f     #f #f  "48;5;3"))
(define tui:fmt:back-blue    (make-tui:fmt #f     #f #f  "48;5;4"))
(define tui:fmt:back-magenta (make-tui:fmt #f     #f #f  "48;5;5"))
(define tui:fmt:back-cyan    (make-tui:fmt #f     #f #f  "48;5;6"))
(define tui:fmt:back-white   (make-tui:fmt #f     #f #f  "48;5;7"))
(define tui:fmt:back+black   (make-tui:fmt #f     #f #f  "48;5;8"))
(define tui:fmt:back+red     (make-tui:fmt #f     #f #f  "48;5;9"))
(define tui:fmt:back+green   (make-tui:fmt #f     #f #f "48;5;10"))
(define tui:fmt:back+yellow  (make-tui:fmt #f     #f #f "48;5;11"))
(define tui:fmt:back+blue    (make-tui:fmt #f     #f #f "48;5;12"))
(define tui:fmt:back+magenta (make-tui:fmt #f     #f #f "48;5;13"))
(define tui:fmt:back+cyan    (make-tui:fmt #f     #f #f "48;5;14"))
(define tui:fmt:back+white   (make-tui:fmt #f     #f #f "48;5;15"))
(define tui:fmt:unback       (make-tui:fmt #f     #f #f    'clear))

(define (tui:with-bold   . parts) (apply tui:string `(,tui:fmt:bold  ,@parts ,tui:fmt:unbold)))
(define (tui:with-under  . parts) (apply tui:string `(,tui:fmt:under ,@parts ,tui:fmt:ununder)))
(define (tui:with-fore f . parts) (apply tui:string `(,f ,@parts ,tui:fmt:unfore)))
(define (tui:with-back b . parts) (apply tui:string `(,b ,@parts ,tui:fmt:unback)))

(module+ main
    (define displaystr (tui:string
        "hello\n"
        (tui:with-bold "1 2 3 4 5 6 7 8 9 0 ")
        (tui:with-under "11 22 33 44 55 66 77 88 99 00 "
            (tui:with-bold "111 222 333 444 555 666 777 888 999 000 "))
        (tui:with-fore tui:fmt:fore-blue
            (tui:with-back tui:fmt:back-white
                "1111    2222    3333    4444    5555    "))
        "6666    7777    8888    9999    0000    "
        (tui:with-back tui:fmt:back+black
            "\nnewline\nnewline                                     "
            "longlineqwertyuiopasdfghjklzxcvbnm")
    ))
    (for ([sd (tui:string-reflow displaystr 20)])
        (displayln sd)))
