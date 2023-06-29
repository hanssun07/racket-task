#lang racket/base

(require
    "task.rkt"
    "user.rkt"
    "utils.rkt"
    racket/match racket/block
    racket/list racket/function
    racket/port racket/pretty
    racket/contract racket/string
    racket/generator
    racket/file
)

(provide
    domain-of domain-register

    make-domain domain?
        domain-id->task
        domain-id->user
        domain-cur-user
        domain-datafile
        set-domain-datafile!
    domain/users domain/tasks
    domain/login
        domain/load
        domain/commit
    domain/register-task
        domain/get-task
        domain/next-task-id
        domain/task-count
    domain/register-user
        domain/get-user-by-id
        domain/get-user-by-name
        domain/next-user-id

    dmpath? (struct-out itempath)
    (struct-out domain-frame)
    domain-tree-root
    register-domain
    current-domain  current-domain-frame
        resolve-domain-frame
        resolve-domain
    in-domain
    select-domain
        select-domain-from-root
    itempath?
        itempath-dmpath itempath-id
        string->itempath    string->dmpath
        dmpath->string)

(struct domain
    (id->task
     id->user
     cur-user
     datafile)
    #:mutable)

(define obj->domain (make-weak-hasheq))
(define (domain-of x) (hash-ref obj->domain x))
(define (domain-register x dm)
    (assert!! (domain? dm))
    (hash-set! obj->domain x dm))

(define atomic-user-id? string?)
(define atomic-task-id? exact-nonnegative-integer?)

(define (make-domain [datafile #f])
    (domain (make-hash) (make-hash) #f datafile))

(define (domain/users dm) (hash-values (domain-id->user dm)))
(define (domain/tasks dm) (hash-values (domain-id->task dm)))

(define (domain/login dm name)
    (set-domain-cur-user! dm (domain/get-user-by-name dm name)))

(define (domain/load dm) (block
    (match-define (domain id->task id->user _ datafile) dm)
    (hash-clear! id->task)
    (hash-clear! id->user)
    (define in (open-input-file datafile))
    (define data (port->list read in))
    (close-input-port in)
    (if (number? (first data))
        (domain/load/v1 dm (cdr data))
        (domain/load/v0 dm data))))
    
(define (domain/load/v1 dm data)
    (match-define (domain _ _ cur-user _) dm)
    (define tasks (first data))
    (define users (second data))
    (for ((task tasks) (id (in-naturals)))
        (domain/register-task dm (datum->task (cons id (cdr task)))))
    (for ((user users) (id (in-naturals)))
        (domain/register-user dm (datum->user (cons id (cdr user)))))
    (when cur-user (domain/login dm (user-id cur-user))))

(define (domain/load/v0 dm data)
    (match-define (domain _ _ cur-user _) dm)
    (define tasks (first data))
    (define users (second data))
    (for ((task tasks))
        (domain/register-task dm (datum->task task)))
    (for ((user users) (id (in-naturals)))
        (domain/register-user dm (datum->user (cons id user))))
    (for ((task (hash-values (domain-id->task dm))))
        (when (task-assigned-to task)
            (task-assign! task (user-id (domain/get-user-by-name dm (task-assigned-to task))))))
    (when cur-user (domain/login dm (user-id cur-user))))

(define (domain/commit dm)
    (match-define (domain id->task id->user cur-user datafile) dm)
    (call-with-atomic-output-file datafile (lambda (out tmppath)
      (parameterize ([current-output-port out])
        (pretty-write 1)
        (pretty-write (map task->datum (sort (hash-values id->task) < #:key task-id)))
        (pretty-write (map user->datum (sort (hash-values id->user) < #:key user-id)))))))

(define (domain/register-task dm t)
    (match-define (domain id->task _ _ _) dm)
    (define id (task-id t))
    (hash-set! id->task id t)
    (domain-register t dm))
(define (domain/get-task dm id)
    (match-define (domain id->task _ _ _) dm)
    (hash-ref id->task id))
(define (domain/next-task-id dm)
    (match-define (domain id->task _ _ _) dm)
    (if (hash-empty? id->task) 0
        (add1 (argmax values (hash-keys id->task)))))

(define (domain/task-count dm)
    (hash-count (domain-id->task dm)))

(define (domain/register-user dm u)
    (match-define (domain _ id->user _ _) dm)
    (define id (user-id u))
    (hash-set! id->user id u)
    (domain-register u dm))
(define (domain/get-user-by-id dm id)
    (match-define (domain _ id->user _ _) dm)
    (hash-ref id->user id))
(define (domain/get-user-by-name dm name
         [failure-result (error-failthrough "no user by name ~a" name)])
    (match-define (domain _ id->user _ _) dm)
    (define matches (filter (lambda (u) (equal? name (user-name u))) (hash-values id->user)))
    (if (empty? matches)
        (if (procedure? failure-result) (failure-result) failure-result)
        (car matches)))
(define (domain/next-user-id dm)
    (match-define (domain _ id->user _ _) dm)
    (if (hash-empty? id->user) 0
        (add1 (argmax values (hash-keys id->user)))))


(struct domain-frame
    (in-domain
     subdomains
     path
     parent)
    #:mutable)
(define domain-tree-root
    (domain-frame
     #f
     (make-hash)
     empty #f))

(define dmpath? (listof symbol?))
(struct itempath (dmpath id) #:transparent)

(define current-domain-frame (make-parameter domain-tree-root))
(define (current-domain) (domain-frame-in-domain (current-domain-frame)))
(define home-domain-frame (make-parameter domain-tree-root))

(define (resolve-domain dmpath [dmf (current-domain-frame)])
    (domain-frame-in-domain (resolve-domain-frame dmpath dmf)))

(define (resolve-domain-frame dmpath [dmf (current-domain-frame)])
    (define (further-resolve dmpath dmf)
        (if (empty? dmpath) dmf (block
            (match-define (domain-frame _ subdomains _ parent) dmf)
            (match-define (cons next rest) dmpath)
            (match next
                [(or '|.| '||) (further-resolve rest dmf)]
                ['.. (further-resolve rest parent)]
                [_
                 (define next-frame (hash-ref subdomains next #f)) 
                 (if next-frame
                    (further-resolve rest next-frame)
                    #f)]))))
    (match dmpath
        ['() dmf]
        [`(~ . ,rest) (further-resolve rest (home-domain-frame))]
        [`(|| . ,rest) (further-resolve rest domain-tree-root)]
        [_ (further-resolve dmpath dmf)]))
(define (resolve-domain-frame! dmpath [dmf (current-domain-frame)])
    (define (further-resolve dmpath dmf)
        (if (empty? dmpath) dmf (block
            (match-define (domain-frame _ subdomains fpath parent) dmf)
            (match-define (cons next rest) dmpath)
            (match next
                [(or '|.| '||) (further-resolve rest dmf)]
                ['.. (further-resolve rest parent)]
                [_
                 (define next-frame (hash-ref subdomains next #f)) 
                 (if next-frame
                    (further-resolve rest next-frame)
                    (block
                        (define next-frame
                            (hash-ref! subdomains next (domain-frame #f (make-hash)
                                                                    (append fpath (list next))
                                                                    dmf)))
                        (further-resolve rest next-frame)))]))))
    (match dmpath
        ['() dmf]
        [`(~ . ,rest) (further-resolve rest (home-domain-frame))]
        [`(|| . ,rest) (further-resolve rest domain-tree-root)]
        [_ (further-resolve dmpath dmf)]))
(define (register-domain dmpath dm [dmf (current-domain-frame)])
    (assert!! (dmpath? dmpath))
    (assert!! (domain? dm))
    (define frame (resolve-domain-frame! dmpath dmf))
    (set-domain-frame-in-domain! frame dm))

(define (in-domain [dmf (current-domain-frame)])
    (in-generator
        (define (rec-run dmf)
            (define chash (domain-frame-subdomains dmf))
            (define ckeys (sort (hash-keys chash) symbol<?))
            (define children (map (curry hash-ref chash) ckeys))
            (yield dmf)
            (for-each rec-run children))
        (rec-run dmf)))

(define (string->itempath str) (block
    (define split (string-split str ":" #:trim? #f))
    (define-values (path id)
        (match split
            ['() (values "" #f)]
            [`(,id) (values "" (if (equal? id "") #f id))]
            [`(,path ,id) (values path (if (equal? id "") #f id))]))
    (define parts (string-split path "/" #:trim? #f))
    (define syms (map string->symbol parts))
    (define parsed-id (and id (or (string->number id) id)))
    (itempath syms parsed-id)))
(define (string->dmpath str) (itempath-dmpath (string->itempath str)))
(define (dmpath->string [dmpath (domain-frame-path (current-domain-frame))])
    (string-append (string-join (map symbol->string dmpath) "/") ":"))

(define (select-domain dmpath [dmf (current-domain-frame)])
    (define selected (resolve-domain-frame dmpath dmf))
    (assert!! selected)
    (current-domain-frame selected))
(define (select-domain-from-root dmpath)
    (select-domain dmpath domain-tree-root))

(module* main #f
    (string->itempath "")
    (string->itempath ":")
    (string->itempath "123")
    (string->itempath "abc:123")
    (string->itempath "xyz/abc:123")
    (string->itempath "xyz/abc:")
    (string->itempath "/xyz/abc:")
    (string->itempath "/:")
    (string->itempath "~:")
)
