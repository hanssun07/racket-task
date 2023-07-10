#lang typed/racket/base

(module domain/base typed/racket/base
    (require
        "task.rkt"  "user.rkt"  "utils.rkt" "types.rkt"
        "utils-untyped.rkt"
        racket/match
        racket/list)
    (provide
        domain-of   domain-register
        Domain  make-domain domain?
            domain-id->task
            domain-id->user
            domain-cur-user
            domain-datafile     set-domain-datafile!
            domain-user-aliases set-domain-user-aliases!
        domain/users domain/tasks
        domain/login
        domain/register-task
            domain/get-task !domain/get-task
            domain/next-task-id
            domain/task-count
        domain/register-user
            domain/get-user-by-id
            domain/get-user-by-name
            domain/next-user-id
            user-display-name)
    (module* internals #f
        (provide domain))
        ;(provide (struct-out domain)))

    (struct domain
        ([id->task      : (Mutable-HashTable Task-ID Task)]
         [id->user      : (Mutable-HashTable User-ID User)]
         [cur-user      : (Option User)]
         [datafile      : (Option Path-String)]
         [user-aliases  : (Immutable-HashTable String String)])
        #:mutable
        #:type-name Domain)

    (: obj->domain (Weak-HashTable Any Domain))
    (: domain-of : Any -> Domain)
    (: domain-register : Any Domain -> Void)
    (define obj->domain (make-weak-hasheq))
    (define (domain-of x) (hash-ref obj->domain x))
    (define (domain-register x dm)
        (hash-set! obj->domain x dm))

    (: make-domain : (case-> (Path-String -> Domain)
                             (            -> Domain)))
    (define (make-domain [datafile : (Option Path-String) #f])
        (domain (make-hash) (make-hash) #f datafile (hash)))

    (: domain/users : Domain -> (Listof User))
    (: domain/tasks : Domain -> (Listof Task))
    (define (domain/users dm) (hash-values (domain-id->user dm)))
    (define (domain/tasks dm) (hash-values (domain-id->task dm)))

    (: domain/login : Domain User -> Void)
    (define (domain/login dm u)
        (set-domain-cur-user! dm u))

    (: domain/register-task : Domain Task -> Void)
    (: domain/get-task (All (F) (case-> (Domain Task-ID        -> (Option Task))
                                        (Domain Task-ID (-> F) -> (U F Task)))))
    (: !domain/get-task : Domain Task-ID -> Task)
    (: domain/next-task-id : Domain -> Task-ID)
    (: domain/task-count : Domain -> Exact-Nonnegative-Integer)
    (define (domain/register-task dm t)
        (define id->task (domain-id->task dm))
        (define id (task-id t))
        (hash-set! id->task id t)
        (domain-register t dm))

    (define domain/get-task (case-lambda
        [(dm id)
         (define id->task (domain-id->task dm))
         (hash-ref id->task id #f)]
        [(dm id on-fail)
         (or (domain/get-task dm id) (on-fail))]))
    (define (!domain/get-task dm id)
        (domain/get-task dm id (errorthunk 'domain/get-task "no task with id ~a." id)))

    (define (domain/next-task-id dm)
        (define id->task (domain-id->task dm))
        (if (hash-empty? id->task) 0
            (add1 ((inst argmax Task-ID) values (hash-keys id->task)))))

    (define (domain/task-count dm)
        (hash-count (domain-id->task dm)))

    (define (domain/register-user [dm : Domain] [u : User]) : Void
        (match-define (domain _ id->user _ _ _) dm)
        (define id (user-id u))
        (hash-set! id->user id u)
        (domain-register u dm))
    (define (domain/get-user-by-id [dm : Domain] [id : User-ID]) : User
        (match-define (domain _ id->user _ _ _) dm)
        (hash-ref id->user id))
    (: domain/get-user-by-name
        (All (T) (->* (Domain String) ((-> T)) (U T User))))
    (define (domain/get-user-by-name dm name
             [failure-result (errorthunk 'domain/get-uesr-by-name "no user by name ~a" name)])
        (match-define (domain _ id->user _ _ _) dm)
        (define matches (filter (lambda ([u : User]) (equal? name (user-display-name u))) (hash-values id->user)))
        (if (empty? matches)
            (failure-result)
            (car matches)))
    (define (domain/next-user-id [dm : Domain]) : User-ID
        (match-define (domain _ id->user _ _ _) dm)
        (if (hash-empty? id->user) 0
            (add1 ((inst argmax User-ID) values (hash-keys id->user)))))

    (: user-display-name (User -> String))
    (define (user-display-name u)
        (or (hash-ref (domain-user-aliases (domain-of u))
                      (user-name u) #f)
            (user-name u)))
)

(module domain-frame/base typed/racket/base
    (require
        (submod ".." domain/base)
        (submod ".." domain/base internals)
        racket/list     racket/function racket/string
        racket/block    racket/match
    )
    (provide
        (struct-out itempath)       ItemPath    DomainPath
        (struct-out domain-frame)   DomainFrame
        domain-tree-root
        register-domain
        current-domain  current-domain-frame
            resolve-domain-frame
            resolve-domain
        select-domain
            select-domain-from-root
        itempath?
            itempath-dmpath itempath-id
            string->itempath    string->dmpath
            dmpath->string)

    (struct domain-frame
        ([in-domain  : (Option Domain)]
         [subdomains : (Mutable-HashTable Symbol DomainFrame)]
         [path       : (Listof Symbol)]
         [parent     : (Option DomainFrame)])
        #:mutable
        #:type-name DomainFrame)
    (define domain-tree-root
        (domain-frame
         #f
         (make-hash)
         empty #f))

    (define-type DomainPath (Listof Symbol))
    (struct itempath
        ([dmpath : DomainPath]
         [id     : Any])
        #:transparent
        #:type-name ItemPath)

    (define current-domain-frame (make-parameter domain-tree-root))
    (define (current-domain) (domain-frame-in-domain (current-domain-frame)))
    (define home-domain-frame (make-parameter domain-tree-root))

    (define (resolve-domain [dmpath : DomainPath] [dmf : DomainFrame (current-domain-frame)]) : Domain
        (assert (domain-frame-in-domain (assert (resolve-domain-frame dmpath dmf)))))

    (: resolve-domain-frame (->* (DomainPath) (DomainFrame) (Option DomainFrame)))
    (define (resolve-domain-frame dmpath [dmf (current-domain-frame)])
        ;(: further-resolve : DomainPath DomainFrame -> (Option DomainFrame))
        (define (further-resolve [dmpath : DomainPath] [dmf : DomainFrame]) : (Option DomainFrame)
            (if (empty? dmpath) dmf (block
                (match-define (domain-frame _ subdomains _ parent) dmf)
                (match-define (cons next rest) dmpath)
                (match next
                    [(or '|.| '||) (further-resolve rest dmf)]
                    ['.. (and parent (further-resolve rest parent))]
                    [_
                     (define next-frame : (Option DomainFrame) (hash-ref subdomains next #f)) 
                     (if next-frame
                        (further-resolve rest next-frame)
                        #f)]))))
        (match dmpath
            ['() dmf]
            [`(~ . ,rest) (further-resolve rest (home-domain-frame))]
            [`(|| . ,rest) (further-resolve rest domain-tree-root)]
            [_ (further-resolve dmpath dmf)]))
    (define (resolve-domain-frame! [dmpath : DomainPath] [dmf : DomainFrame (current-domain-frame)]) : DomainFrame
        (define (further-resolve [dmpath : DomainPath] [dmf : DomainFrame]) : DomainFrame
            (if (empty? dmpath) dmf (block
                (match-define (domain-frame _ subdomains fpath parent) dmf)
                (match-define (cons next rest) dmpath)
                (match next
                    [(or '|.| '||) (further-resolve rest dmf)]
                    ['.. (further-resolve rest (assert parent))]
                    [_
                     (define next-frame (hash-ref subdomains next #f)) 
                     (if next-frame
                        (further-resolve rest next-frame)
                        (block
                            (define next-frame
                                ((inst hash-ref! Symbol DomainFrame)
                                       subdomains next (thunk (domain-frame #f (make-hash)
                                                                        (append fpath (list next))
                                                                        dmf))))
                            (further-resolve rest next-frame)))]))))
        (match dmpath
            ['() dmf]
            [`(~ . ,rest) (further-resolve rest (home-domain-frame))]
            [`(|| . ,rest) (further-resolve rest domain-tree-root)]
            [_ (further-resolve dmpath dmf)]))

    (define (register-domain [dmpath : DomainPath] [dm : Domain] [dmf : DomainFrame (current-domain-frame)])
        (define frame (resolve-domain-frame! dmpath dmf))
        (set-domain-frame-in-domain! frame dm))

    (define (string->itempath [str : String]) : ItemPath (block
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
    (define (string->dmpath [str : String]) (itempath-dmpath (string->itempath str)))
    (define (dmpath->string [dmpath : DomainPath (domain-frame-path (current-domain-frame))]) : String
        (string-append (string-join (map symbol->string dmpath) "/") ":"))

    (define (select-domain [dmpath : DomainPath] [dmf : DomainFrame (current-domain-frame)])
        (define selected (resolve-domain-frame dmpath dmf))
        (assert selected)
        (current-domain-frame selected))
    (define (select-domain-from-root [dmpath : DomainPath])
        (select-domain dmpath domain-tree-root))
)

(module domain/repo racket/base
    (require
        (submod ".." domain/base)
        (submod ".." domain/base internals)
        (submod ".." domain-frame/base)
        "task.rkt"  "user.rkt"
        racket/block    racket/match
        racket/port     racket/file     racket/pretty
        racket/list
)
    (provide
        domain/load domain/commit)

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

    (define (domain/commit dm)
        (match-define (domain id->task id->user cur-user datafile _) dm)
        (call-with-atomic-output-file datafile (lambda (out tmppath)
          (parameterize ([current-output-port out])
            (pretty-write 1)
            (pretty-write (map task->datum (sort (hash-values id->task) < #:key task-id)))
            (pretty-write (map user->datum (sort (hash-values id->user) < #:key user-id)))))))
)

(module domain-frame/traverse racket/base
    (require
        (submod ".." domain-frame/base)
        racket/function
        racket/generator)
    (provide in-domain)

    (define (in-domain [dmf (current-domain-frame)])
        (in-generator
            (define (rec-run dmf)
                (define chash (domain-frame-subdomains dmf))
                (define ckeys (sort (hash-keys chash) symbol<?))
                (define children (map (curry hash-ref chash) ckeys))
                (yield dmf)
                (for-each rec-run children))
            (rec-run dmf)))
)

(require
    'domain/base
    'domain-frame/base)
(provide
    (all-from-out 'domain/base)
    (all-from-out 'domain-frame/base))

(require/typed/provide 'domain/repo
    [domain/load    (Domain -> Void)]
    [domain/commit  (Domain -> Void)])
(require/typed/provide 'domain-frame/traverse
    [in-domain      (->* () (DomainFrame) (Sequenceof DomainFrame))])

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
