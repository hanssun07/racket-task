#lang racket/base

(require
    "utils/ann.rkt"
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
        domain-datafile     set-domain-datafile!
        domain-user-aliases set-domain-user-aliases!
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
        user-display-name

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

(:structdef domain : Domain
    ([id->task     : (MHash TaskId Task)]
     [id->user     : (MHash UserId User)]
     [cur-user     : (? User)]
     [datafile     : (? PathString)]
     [user-aliases : (MHash String String)]))
(struct domain
    (id->task
     id->user
     cur-user
     datafile
     user-aliases)
    #:mutable)

(: obj->domain (MHashWeakEq Any Domain))
(: domain-of (Any -> (U Domain (^ Exn:Fail))))
(: domain-register (Any Domain -> Void))
(define obj->domain (make-weak-hasheq))
(define (domain-of x) (hash-ref obj->domain x))
(define (domain-register x dm)
    (assert!! (domain? dm))
    (hash-set! obj->domain x dm))

(: make-domain
    (String -> Domain)
    (       -> Domain))
(define (make-domain [datafile #f])
    (domain (make-hash) (make-hash) #f datafile (hash)))

(: domain/users (Domain -> (Listof User)))
(: domain/tasks (Domain -> (Listof Task)))
(define (domain/users dm) (hash-values (domain-id->user dm)))
(define (domain/tasks dm) (hash-values (domain-id->task dm)))

(: domain/login (Domain User -> Void))
(define (domain/login dm u)
    (set-domain-cur-user! dm u))

(: domain/load (Domain -> Void))
(define (domain/load dm) (block
    (match-define (domain id->task id->user _ datafile _) dm)
    (hash-clear! id->task)
    (hash-clear! id->user)
    (define in (open-input-file datafile))
    (define data (port->list read in))
    (close-input-port in)
    (if (number? (first data))
        (domain/load/v1 dm (cdr data))
        (domain/load/v0 dm data))))

(define (domain/load/v1 dm data)
    (match-define (domain _ _ cur-user _ _) dm)
    (define tasks (first data))
    (define users (second data))
    (for ((task tasks) (id (in-naturals)))
        (domain/register-task dm (datum->task (cons id (cdr task)))))
    (for ((user users) (id (in-naturals)))
        (domain/register-user dm (datum->user (cons id (cdr user)))))
    (when cur-user (domain/login dm (user-id cur-user))))

(define (domain/load/v0 dm data)
    (match-define (domain _ _ cur-user _ _) dm)
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

(: domain/commit (Domain -> Void))
(define (domain/commit dm)
    (match-define (domain id->task id->user cur-user datafile _) dm)
    (call-with-atomic-output-file datafile (lambda (out tmppath)
      (parameterize ([current-output-port out])
        (pretty-write 1)
        (pretty-write (map task->datum (sort (hash-values id->task) < #:key task-id)))
        (pretty-write (map user->datum (sort (hash-values id->user) < #:key user-id)))))))

(: domain/register-task (Domain Task -> Void))
(: domain/get-task (Domain TaskId -> (U Task (^ Exn:Fail))))
(: domain/next-task-id (Domain -> TaskId))
(define (domain/register-task dm t)
    (match-define (domain id->task _ _ _ _) dm)
    (define id (task-id t))
    (hash-set! id->task id t)
    (domain-register t dm))
(define (domain/get-task dm id)
    (match-define (domain id->task _ _ _ _) dm)
    (hash-ref id->task id))
(define (domain/next-task-id dm)
    (match-define (domain id->task _ _ _ _) dm)
    (if (hash-empty? id->task) 0
        (add1 (argmax values (hash-keys id->task)))))

(: domain/task-count (Domain -> ExactNonnegativeInteger))
(define (domain/task-count dm)
    (hash-count (domain-id->task dm)))

(: domain/register-user (Domain User -> Void))
(: domain/get-user-by-id (Domain UserId -> (U User (^ Exn:Fail))))
(: domain/get-user-by-name
    (Domain String (-> f) -> (U User f))
    (Domain String f      -> (U User f))
    (Domain String        -> (U User (^ Exn:Fail))))
(: domain/next-user-id (Domain -> UserId))
(define (domain/register-user dm u)
    (match-define (domain _ id->user _ _ _) dm)
    (define id (user-id u))
    (hash-set! id->user id u)
    (domain-register u dm))
(define (domain/get-user-by-id dm id)
    (match-define (domain _ id->user _ _ _) dm)
    (hash-ref id->user id))
(define (domain/get-user-by-name dm name
         [failure-result (error-failthrough "no user by name ~a" name)])
    (match-define (domain _ id->user _ _ _) dm)
    (define matches (filter (lambda (u) (equal? name (user-display-name u))) (hash-values id->user)))
    (if (empty? matches)
        (if (procedure? failure-result) (failure-result) failure-result)
        (car matches)))
(define (domain/next-user-id dm)
    (match-define (domain _ id->user _ _ _) dm)
    (if (hash-empty? id->user) 0
        (add1 (argmax values (hash-keys id->user)))))

(: user-display-name (User -> String))
(define (user-display-name u)
    (hash-ref (domain-user-aliases (domain-of u))
              (user-name u)
              (user-name u)))


(:structdef domain-frame : DomainFrame
    ([in-domain  : (? Domain)]
     [subdomains : (MHash Symbol DomainFrame)]
     [path       : (Listof Symbol)]
     [parent     : (? DomainFrame)]))
(: domain-tree-root DomainFrame)
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

(:typedef DomainPath (Listof Symbol))
(:structdef itempath : ItemPath
    ([dmpath : Domainpath]
     [id     : Any]))
(define dmpath? (listof symbol?))
(struct itempath (dmpath id) #:transparent)

(: current-domain-frame
    (Parameter DomainFrame)
    (-> DomainFrame)
    (DomainFrame -> Void))
(: current-domain (-> Domain))
(: home-domain-frame
    (Parameter DomainFrame)
    (-> DomainFrame)
    (DomainFrame -> Void))
(define current-domain-frame (make-parameter domain-tree-root))
(define (current-domain) (domain-frame-in-domain (current-domain-frame)))
(define home-domain-frame (make-parameter domain-tree-root))

(: resolve-domain
    (DomainPath DomainFrame -> (U Domain (^ Exn:Fail)))
    (DomainPath             -> (U Domain (^ Exn:Fail))))
(define (resolve-domain dmpath [dmf (current-domain-frame)])
    (domain-frame-in-domain (resolve-domain-frame dmpath dmf)))

(: resolve-domain-frame
    (DomainPath DomainFrame -> (U DomainFrame (^ Exn:Fail)))
    (Domainpath             -> (U DomainFrame (^ Exn:Fail))))
(: resolve-domain-frame!
    (DomainPath DomainFrame -> (U DomainFrame (^ Exn:Fail)))
    (Domainpath             -> (U DomainFrame (^ Exn:Fail))))
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

(: register-domain
    (DomainPath Domain DomainFrame -> Void)
    (Domainpath Domain             -> Void))
(define (register-domain dmpath dm [dmf (current-domain-frame)])
    (assert!! (dmpath? dmpath))
    (assert!! (domain? dm))
    (define frame (resolve-domain-frame! dmpath dmf))
    (set-domain-frame-in-domain! frame dm))

(: in-domain
    (DomainFrame -> (Sequenceof DomainFrame))
    (            -> (Sequenceof DomainFrame)))
(define (in-domain [dmf (current-domain-frame)])
    (in-generator
        (define (rec-run dmf)
            (define chash (domain-frame-subdomains dmf))
            (define ckeys (sort (hash-keys chash) symbol<?))
            (define children (map (curry hash-ref chash) ckeys))
            (yield dmf)
            (for-each rec-run children))
        (rec-run dmf)))

(: string->itempath (String -> (U ItemPath (^ Exn:Fail))))
(: string->dmpath (String -> (U DomainPath (^ Exn:Fail))))
(: dmpath->string (DomainPath -> String))
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

(: select-domain
    (DomainPath DomainFrame -> Void)
    (DomainPath             -> Void))
(: select-domain-from-root (DomainPath -> Void))
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