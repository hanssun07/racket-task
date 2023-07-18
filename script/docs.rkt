#lang racket/base

(require
    "../src/repl/utils.rkt"
    racket/port racket/list racket/file racket/function
    racket/path racket/pretty racket/syntax racket/format
    racket/hash
    racket/match
)

(struct record
    (name in-module attrs);public? args types)
    #:transparent)
(define (record-attr-ref rec attr) (hash-ref (record-attrs rec) attr #f))
(define (record-attr-set! rec attr v) (hash-set! (record-attrs rec) attr v))

(define current-module-name (make-parameter ""))
(define cmn current-module-name)

(define (parse-recs datum)
    (match datum
        [`(: ,name ,types ...)
         (list (record name (cmn) ;#f '() types))]
                       (hash 'types types)))]
        [`(:typedef (,name ,params) ,type)
         (list (record name (cmn) ;#t (list params) (list type)))]
                       (hash 'provided #t
                             'partypedef #t
                             'params params
                             'types (list type))))]
        [`(:typedef ,name ,type)
         (list (record name (cmn) ;#t '() (list type)))]
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
                  (record tname (cmn) ;#f '() '(struct))
                          (hash 'struct sname
                                'provided #t
                                'typedef #t
                                'types `((,_tname ,@ftypes))))
                  (record sname (cmn) ;#f `(,fnames) `((,ftypes -> ,tname))))
                          (hash 'args fnames
                                'types `((,@ftypes -> ,tname)))))
            (map (lambda (fname ftype) 
                  (record (format-symbol "~a-~a" sname fname) (cmn); #f 
                          ;`(,sname) `((,tname -> ,ftype))))
                          (hash 'args (list sname)
                                'types `((,tname -> ,ftype)))))
                fnames ftypes)
            (map (lambda (fname ftype)
                  (record (format-symbol "set-~a-~a!" sname fname) (cmn); #f
                          ;`(,sname ,fname) `((,tname ,ftype -> Void))))
                          (hash 'args (list sname fname)
                                'types `((,tname ,ftype -> Void)))))
                fnames ftypes))]
        [`(provide ,vals ...)
         (map (lambda (name) (record name (cmn) ;#t '() '()))
                                     (hash 'provided #t)))
            vals)]
        [`(define (,name ,args ...) ,_ ...)
         (define (formal->argname arg) (if (cons? arg) (car arg) arg))
         (define (formal->default arg) (if (cons? arg) (cadr arg) ""))
         (list (record name (cmn) ;#f args '()))]
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
        (match-define (record a-name a-in-module a-attrs) a);a-public? a-args a-types) a)
        (match-define (record b-name b-in-module b-attrs) b);b-public? b-args b-types) b)
        (if (and (equal? a-name b-name) (equal? a-in-module b-in-module))
            (list (record a-name a-in-module ;(or a-public? b-public?)
                                             ;(append a-args b-args)
                                             ;(append a-types b-types)))
                          (hash-union a-attrs b-attrs #:combine/key combiner)))
                          
            (list a b)))
    (foldr (lambda (x xs) (if (empty? xs) (list x) (append (merge x (car xs)) (cdr xs))))
        empty sorted))

(define (from-file filename)
    (define in (open-input-file filename))
    (define data (begin (read-line in) (port->list read in)))
    (close-input-port in)
    (parameterize ([current-module-name (path->string filename)])
        (apply append (map parse-recs data))))
    
(define recs (apply append (map from-file (find-files (curryr path-has-extension? ".rkt") "src"))))
(define filtered (filter (compose symbol? record-name) recs))
(define merged (merge-recs filtered))


(define ansi/bold "\033[1m")
(define ansi/reset "\033[0m")
(define (list-extend lst len (val ""))
    (take (append lst (make-list len val)) len))
(define (anytable->strtable x)
    (map (lambda (x) (map ~a x)) x))
;(define (rec->string x)
;    (if (list? x) (map rec->string x) (~a x)))
(define (displayrec rec)
    (match-define (record name in-module _) rec);public? args types) rec)
    (define args (record-attr-ref rec 'args))
    (define types (record-attr-ref rec 'types))
    (printf "~a~a~a, ~a~a\n" ansi/bold name ansi/reset in-module
        (if (record-attr-ref rec 'provided?);public?
             "" " (private)"))
    ;(when args (display "    ") (displayln args))
    ;(when types (for-each (lambda (x) (display "    ") (displayln x)) types))
    ;(printf "~a\n~a\n" args types)
    (when (and args types)
        (displayfn-args+types rec))
    (newline))
(define (displayfn-args+types rec)
    (match-define (record name in-module _) rec);public? args types) rec)
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
    (define rows-jagged ((if defaults? (curry cons defline) values)
                         (cons argline typelines)))
    (define rows-maxlen (length (argmax length rows-jagged)))
    (define rows (map (lambda (x) (list-extend x rows-maxlen)) rows-jagged))
    (print-table (anytable->strtable rows)
        (make-list rows-maxlen 1)
        (make-list rows-maxlen 100)
        (cons 0 (make-list (sub1 rows-maxlen) 1))
        (make-list rows-maxlen 'left)))
    
#|
    (when (and args types)
        (define rows (anytable->strtable (if (empty? args) types (cons args types))))
        (define maxlen (length (argmax length rows)))
        (define fixed (map (lambda (x) (list-extend x maxlen)) rows))
        (print-table fixed
            (make-list maxlen 1)
            (make-list maxlen 100)
            (cons 2 (make-list (sub1 maxlen) 1))
            (make-list maxlen 'left)))
)|#
(for-each (lambda (x) (printf "~a " (record-name x))) merged)
(newline)
(writeln (car (filter (lambda (x) (equal? 'Cardinal (record-name x))) merged)))
(for-each displayrec merged)




