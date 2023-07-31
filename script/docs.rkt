#lang racket/base

(require
    "../src/repl/utils.rkt"
    racket/port racket/list racket/file racket/function
    racket/path racket/pretty racket/syntax racket/format
    racket/hash
    racket/match
    racket/cmdline
)

(struct record
    (name in-module attrs)
    #:transparent)
(define (record-attr-ref rec attr) (hash-ref (record-attrs rec) attr #f))
(define (record-attr-set! rec attr v) (hash-set! (record-attrs rec) attr v))

(define current-module-name (make-parameter ""))
(define cmn current-module-name)

(define (parse-recs datum)
    (match datum
        [`(: ,name ,types ...)
         (list (record name (cmn)
                       (hash 'types types)))]
        [`(:typedef (,name ,params) ,type)
         (list (record name (cmn)
                       (hash 'provided #t
                             'partypedef #t
                             'params params
                             'types (list type))))]
        [`(:typedef ,name ,type)
         (list (record name (cmn)
                       (hash 'provided #t
                             'typedef #t
                             'types (list type))))]
        [`(:structdef ,sname : ,tname ([,fnames : ,ftypes] ...))
         (define _tname (format-symbol "_~a" tname))
         (append
            (list (record _tname (cmn)
                          (hash 'provided #t
                                'partypedef #t
                                'params fnames
                                'types `((,_tname ,@fnames))))
                  (record tname (cmn)
                          (hash 'struct sname
                                'provided #t
                                'typedef #t
                                'types `((,_tname ,@ftypes))))
                  (record sname (cmn)
                          (hash 'args fnames
                                'types `((,@ftypes -> ,tname)))))
            (map (lambda (fname ftype) 
                  (record (format-symbol "~a-~a" sname fname) (cmn)
                          (hash 'args (list sname)
                                'types `((,tname -> ,ftype)))))
                fnames ftypes)
            (map (lambda (fname ftype)
                  (record (format-symbol "set-~a-~a!" sname fname) (cmn)
                          (hash 'args (list sname fname)
                                'types `((,tname ,ftype -> Void)))))
                fnames ftypes))]
        [`(provide ,vals ...)
         (map (lambda (name) (record name (cmn)
                                     (hash 'provided #t)))
            vals)]
        [`(define (,name ,args ...) ,_ ...)
         (define (formal->argname arg) (if (cons? arg) (car arg) arg))
         (define (formal->default arg) (if (cons? arg) (cadr arg) ""))
         (list (record name (cmn)
                       (hash 'args (map formal->argname args)
                             'defaults (map formal->default args))))]
        [_ '()]))
            
(define (merge-recs recs)
    (define sorted (sort recs symbol<? #:key record-name))
    (define (combiner k a b)
        (case k
            [(types) (append a b)]
            [else (or a b)]))
    (define (merge a b)
        (match-define (record a-name a-in-module a-attrs) a)
        (match-define (record b-name b-in-module b-attrs) b)
        (if (and (equal? a-name b-name) (equal? a-in-module b-in-module))
            (list (record a-name a-in-module
                          (hash-union a-attrs b-attrs #:combine/key combiner)))
            (list a b)))
    (foldr (lambda (x xs) (if (empty? xs) (list x) (append (merge x (car xs)) (cdr xs))))
        empty sorted))

(define (from-file filename)
    (define in (open-input-file filename))
    (define data (begin (read-line in) (port->list read in)))
    (close-input-port in)
    (parameterize ([current-module-name filename])
        (apply append (map parse-recs data))))
    
(define ansi/bold "\033[1m")
(define ansi/reset "\033[0m")
(define (list-extend lst len (val ""))
    (take (append lst (make-list len val)) len))
(define (anytable->strtable x)
    (map (lambda (x) (map ~a x)) x))
(define (with-output-indented pre th) (block
    (define str (open-output-string))
    (parameterize ([current-output-port str]) (th))
    (define lines (string-split (get-output-string str)
                                "\n" #:trim? #f #:repeat? #t))
    (for ([line lines]) (printf "~a~a\n" pre line))))
(define (displayrec pre rec)
    (match-define (record name in-module _) rec)
    (define args (record-attr-ref rec 'args))
    (define types (record-attr-ref rec 'types))
    (printf "   ~a~a" in-module (if (record-attr-ref rec 'provided?) "" " (private)"))
    (printf "~a ~a~a~a"
         (round (- 99 (* 99 pre)))
         ansi/bold name ansi/reset)
    (with-output-indented "   " (thunk
        (when (and args types)
            (displayfn-args+types rec))))
    (newline))
(define (displayfn-args+types rec)
    (match-define (record name in-module _) rec)
    (define args (record-attr-ref rec 'args))
    (define types (record-attr-ref rec 'types))
    (define defaults (record-attr-ref rec 'defaults))
    (define defline (cons "" defaults))
    (define defaults? (and defaults (memf (lambda (x) (not (equal? "" x))) defaults)))
    (define argline (if (empty? args)
                        (list (format "(~a)" name))
                        (cons (format "(~a" name)
                            (reverse (cons (format "~a)" (car (reverse args)))
                                            (cdr (reverse args)))))))
    (define arglen (length argline))
    (define fntypes (filter (curry member '->) types))
    (define argtypess (map (lambda (xs) (takef xs (lambda (x) (not (eq? '-> x))))) fntypes))
    (define rettypes (map (lambda (xs) (dropf xs (lambda (x) (not (eq? '-> x))))) fntypes))
    (define typelines-args (map (lambda (xs) (list-extend (cons "" xs) arglen)) argtypess))
    (define typelines (map append typelines-args rettypes))
    (define rows-jagged (reverse ((if defaults? (curry cons defline) values)
                                 (cons argline typelines))))
    (define rows-maxlen (length (argmax length rows-jagged)))
    (define rows (map (lambda (x) (list-extend x rows-maxlen)) rows-jagged))
    (print-table (anytable->strtable rows)
        (make-list rows-maxlen 1)
        (make-list rows-maxlen 100)
        (cons 2 (make-list (sub1 rows-maxlen) 1))
        (make-list rows-maxlen 'left)))
    
(define default-files (map path->string (find-files (curryr path-has-extension? ".rkt") "src")))

(define override-default-files? (make-parameter #f))
(define search-type (make-parameter 'fuzzy))
(define files (make-parameter default-files))
(define num-results (make-parameter 5))

(define to-search
    (command-line
     #:multi
     [("-f" "--source") file
      "Search this file instead of the default."
      (unless (override-default-files?)
          (override-default-files? #t)
          (files '()))
      (files (cons file (files)))]
     #:once-any
     ["-n" n
      "Set how many entries to show (default 5)."
      (num-results n)]
     [("-a" "--all")
      "Show all entries."
      (num-results (in-naturals))]
     #:once-any
     [("-e" "--exact")
      "Use an exact search instead of a fuzzy search."
      (search-type 'exact)]
     [("-z" "--fuzzy")
      "Use a fuzzy search based on Levenshtein distance (default)."
      (search-type 'fuzzy)]
     #:args terms terms))

(define recs (apply append (map from-file (files))))
(define filtered (filter (compose symbol? record-name) recs))
(define merged (merge-recs filtered))

(require levenshtein)
(define (sqr x) (* x x))
(define dist (case (search-type)
    [(fuzzy)
     (lambda (x y) (/ (sqr (string-levenshtein x y)) (string-length x) (string-length y)))]
    [(exact)
     (lambda (x y) (if (equal? x y) 0 1))]))

(define (dist-fn rec)
    (define name (symbol->string (record-name rec)))
    (apply min 100 (map (curry dist name) to-search)))
(define decorated (map (lambda (x) (cons x (dist-fn x))) merged))
(define sorted (sort decorated < #:key cdr))
    
(for ([rec (in-list sorted)] [_ (num-results)])
    (printf "~a " (round (- 99 (* 99 (cdr rec)))))
    (displayrec (car rec)))
            
