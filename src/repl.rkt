#lang racket/base

(require
    "task.rkt"
    "user.rkt"
    "utils.rkt"
    "model.rkt"
    "domain.rkt"
    "repl/utils.rkt"
    racket/block racket/match racket/exn racket/function
    racket/string racket/list racket/format racket/pretty
    racket/port racket/file racket/system)
(provide load commit login repl)

(define-syntax-rule
    (retry-until-success body ...)
    (let loop ()
        (let* ([failed? #f]
               [res (with-handlers
                        ([exn:break? raise]
                         [(const #t) (lambda (x) (eprintf "~a" (exn->string x))
                                                 (set! failed? #t))])
                        body ...)])
            (if failed? (loop) res))))
(define (prompt [str #f])
    (when str (display str))
    (printf "> ")
    (flush-output))
(define (format-date dt)
    (define (~00 x) (~r x #:min-width 2 #:pad-string "0"))
    (match-define (date _ _ _ d m y _ _ _ _) (seconds->date dt))
    (format "~a-~a-~a" y (~00 m) (~00 d)))
(define (prompt-multi)
    (displayln "Empty line terminates input.")
    (prompt)
    (let loop ((lines empty))
        (define ln (read-line))
        (if (zero? (string-length ln))
            (string-join (reverse lines))
            (begin (prompt)
                   (loop (cons ln lines))))))
(define (prompt-editor str) (block
    (define tmp (make-temporary-file))
    (display-to-file str tmp #:exists 'replace)
    (system (format "$EDITOR \"~a\"" tmp))
    (define res (file->string tmp))
    (delete-file tmp)
    res))
(define (read-token) (symbol->string (read)))
(define (read-line-tokens [in-port (current-input-port)])
    (define ln (read-line in-port))
    (define in (open-input-string ln "user input"))
    (define tks (port->list read in))
    (close-input-port in)
    (map (lambda (x) (if (symbol? x) (symbol->string x) x))
         tks))
(define (save-and-exit) (commit) (exit))
(define (eof-barrier [action save-and-exit]) (when (eof-object? (peek-char)) (action)))
    
(define datafile "data.dat")
(define swapfile "data.swp")

(define (load) (domain/load (current-domain)))
(define (commit) (domain/commit (current-domain)))

(define (login) (let/cc return
    (retry-until-success (block
        (prompt "login")
        (eof-barrier (thunk* (return #f)))
        (define argv (read-line-tokens))
        (define argc (length argv))
        (assert!! (= 1 argc))
        (domain/login (current-domain) (car argv))
        (printf "Logged in as ~a.\n" (car argv))
        #t))))

(define (handle-repl argc argv)
    (define continue? #t)
    (match (car argv)
        [(or "help" "?")
         (help)]
        [(or "cat" "view-task" "view")
         (assert!! (= 2 argc))
         (show-task (second argv))]
        [(or "ed" "edit-task" "edit")
         (assert!! (<= 2 argc))
         (if (= argc 2) 
             (edit-task (second argv))
             (handle-edit-task (second argv) (cddr argv)))]
        [(or "e" "eval")
         (assert!! (= 1 argc))
         (define tasks-to-eval
            (query-tasks (sort-by < task-id)
                         (filter-by (curry user-needs-eval-task? (get-user-me)))))
         (eval-tasks tasks-to-eval)]
        [(or "u" "user" "new-user")
         (assert!! (= 2 argc))
         (new-user (second argv))]
        [(or "n" "new" "new-task")
         (assert!! (= 2 argc))
         (new-task (second argv))]
        [(or "commit")
         (commit)]
        [(or "reload")
         (load)]
        [(or "swap-user")
         (login)]
        [(or "l" "ls" "ll" "list") (block
         (define filters (list
            (filter-by task-ready?)
            (filter-by not task-done?)))
         (define (add-filter . args) (set! filters (cons (apply filter-by args) filters)))
         (define by-priority (sort-by > (curry get-user-task-assignment-index (get-user-me))))
         (define by-id (sort-by < task-id))
         (define sorter (list by-id by-priority))
         (define n (in-naturals 0))
         (for ((arg (cdr argv)))
            (match arg
                ["blocked" (add-filter not task-ready?)]
                ["pending" (add-filter task-ready?) (add-filter not task-assigned?)]
                [(or "all" "-a") (set! filters empty)]
                ["ready" (add-filter task-ready?)]
                ["assigned" (add-filter task-assigned?)]
                ["unassigned" (add-filter not task-assigned?)]
                ["done" (add-filter task-done?)]
                ["not-done" (add-filter not task-done?)]
                ["mine" (add-filter (curry equal? (user-id (get-user-me))) task-assigned-to)]
                [(? number?) (set! n arg)]
                [(or "by-id" "-t") (set! sorter (list by-id))]
                [(or "by-priority" "-p") (set! sorter (list by-id by-priority))]))
         (define tasks (list-truncate (apply query-tasks (append sorter filters)) n))
         (list-tasks tasks)
         (printf "~a/~a tasks shown.\n" (length tasks) (task-count)))]
        [(or "cd" "change-domain")
         (assert!! (<= argc 2))
         (define path 
            (match argv
                [`(,_ "/") "~:"]
                [`(,_ ,str) (string-append str ":")]
                [_ "~:"]))
         (select-domain (itempath-dmpath (string->itempath path)))]
        [(or "ld" "list-domains")
         (for ([dmf (in-domain domain-tree-root)])
            (displayln (dmpath->string (domain-frame-path dmf))))]
        [(or #f "sum" "summary")
         (define tasks-assigned (apply append
            (for/list ([dmf (in-domain)]) (parameterize ([current-domain-frame dmf])
                (define tasks
                    (if (get-user-me)
                        (query-tasks (sort-by < task-id)
                                     (filter-by (curry equal? (user-id (get-user-me))) task-assigned-to)
                                     (filter-by task-assigned?)
                                     (filter-by not task-done?))
                        empty))
                (define dmstr (dmpath->string (domain-frame-path dmf)))
                (map (curry cons dmstr)
                     (map task->summaryrow tasks))))))
         (define tasks-awaiting-eval (apply append
            (for/list ([dmf (in-domain)])
                (parameterize ([current-domain-frame dmf])
                    (if (get-user-me)
                        (query-tasks (filter-by (curry user-needs-eval-task? (get-user-me))))
                        empty)))))
         (define num-tasks-pending 0)
         (define tasks-pending (apply append
            (for/list ([dmf (in-domain)]) (parameterize ([current-domain-frame dmf])
                (define tasks
                    (if (get-user-me)
                        (query-tasks (sort-by < task-id)
                                     (sort-by > (curry get-user-task-assignment-index (get-user-me)))
                                     (filter-by task-ready?)
                                     (filter-by not task-assigned?))
                        empty))
                (define dmstr (dmpath->string (domain-frame-path dmf)))
                (set! num-tasks-pending (+ num-tasks-pending (length tasks)))
                (map (curry cons dmstr)
                     (map task->summaryrow (list-truncate tasks 5)))))))
         (define tab (append
            (if (empty? tasks-assigned)
                (list (list "" "" "" "" "\rNo tasks in progress."))
                (list (list "" "" "" "" (format "\r~a tasks in progress." (length tasks-assigned)))))
            tasks-assigned
            (if (empty? tasks-awaiting-eval)
                empty
                (list (list "" "" "" "" (format "\r~a tasks awaiting evaluation." (length tasks-awaiting-eval)))))
            (if (zero? num-tasks-pending)
                (list (list "" "" "" "" "\rNo tasks pending."))
                (list (list "" "" "" "" (format "\r~a tasks pending." num-tasks-pending))))
            tasks-pending))
         (print-table tab
            '(1 1 20 4 0)
            '(20 5 60 4 1000)
            '(2 0 1 1 2)
            '(right right left left left))]
        [(or "q" "quit")
         (commit)
         (set! continue? #f)]
        [(or "q!" "exit")
         (set! continue? #f)])
    continue?)

(define (repl) (let/cc return
    (define continue? #t)
    (unless (or (get-user-me) (login)) (return #f))
    (retry-until-success (block
        (prompt (format "~a@~a" (user-id (get-user-me)) (dmpath->string)))
        (eof-barrier)
        (define argv (read-line-tokens))
        (define argc (length argv))
        (assert!! (< 0 argc))
        (match-define (itempath cmd-dom cmd) (string->itempath (car argv)))
        (if (empty? cmd-dom)
            (set! continue? (and (handle-repl argc (cons cmd (cdr argv))) continue?))
            (parameterize ([current-domain-frame (resolve-domain-frame cmd-dom)])
                (set! continue? (and (handle-repl argc (cons cmd (cdr argv))) continue?))))
        (when continue? (repl))))))

(define (help)
    (define helpmsg (list
        "?   help                   view this message"
        "    reload                 recover the state from the last commit"
        "    commit                 commit changes to file"
        "q   quit                   quit, committing changes"
        "q!  exit                   quit without committing changes"
        ":   summary                list a summary of assigned and pending tasks"
        "ls  list [opts ...]        list tasks..."
        "     -a all                    default is all pending and in-progress tasks"
        "        ready blocked          require or exclude ready tasks"
        "        assigned unassigned    require or exclude assigned tasks"
        "        done not-done          require or exclude done tasks"
        "        mine                   require assigned to me"
        "        <number>               maximum to show"
        "     -t by-id                  order by ascending id"
        "     -p by-priority            order by descending priority score"
        "                           options trigger left-to-right"
        "cat view-task <id>         view details of a single task"
        "ed  edit-task <id>         enter editing mode for a task"
        "              <id> [<cmd>]"
        "                           run <cmd> in editing <id>"
        "e   eval                   enter evaluation mode"
        "n   new-task \"<name>\"    create a new task"
        "u   new-user <name>        create a new user"
        "    swap-user              login to a different user"
        ""
        "cd  change-domain <domain> change the current domain"
        "      ~ / <empty>              to the root domain"
        "ld  list-domains           see all domains"
        "<dompath>:<cmd>            run <cmd> in domain relative to <dompath>"))
    (for-each displayln helpmsg))

(define (new-user name)
    (assert!! (not (hash-has-key? (domain-id->user (current-domain)) name)))
    (register-user (make-user name))
    (printf "User ~a registered.\n" name))

(define (new-task name)
    (define id (next-task-id))
    (register-task (make-task id name))
    (printf "Registered task ~a: ~a.\n" id name))

(define (show-task id)
    (define t (get-task id))
    (newline)
    (printf "Task ~a: ~a\n" (task-id t) (task-title t))
    (if (task-desc t)
        (printf "~a\n\n" (task-desc t))
        (printf "[no description]\n\n"))
    (when (task-ready? t)
        (printf "  Ready ~a.\n" (format-date (task-ready-by t))))
    (when (task-assigned? t)
        (define u (task-assigned-to t))
        (printf "  Assigned ~a to ~a.\n" (format-date (task-started-by t)) u))
    (when (task-done? t) 
        (printf "  Completed ~a.\n" (format-date (task-done-by t))))
    (unless (task-ready? t)
        (printf "  Pending..."))
    (unless (task-assigned? t)
        (define rates (hash-ref (get-task-evals t) (user-id (get-user-me))))
        (define priority (get-task-priority t))
        (printf "    priority #~a (~a)\n"
            (add1 (length (filter (lambda (t) (if priority (> (get-task-priority t) priority) #t))
                                  (query-tasks (filter-by task-ready?)
                                               (filter-by not task-assigned?)
                                               (filter-by get-task-priority)))))
            priority)
        (printf "    your ratings:\n")
        (printf "      ~a/4 interest\n" (or (uteval-interest rates) '-))
        (printf "      ~a/4 priority\n" (or (uteval-priority rates) '-))
        (printf "      ~a   needs refinement?\n" (or (uteval-needs-refinement rates) '-)))
    (newline))

(define (handle-edit-task id argv)
    (define t (get-task id))
    (define continue? #t)
    (define argc (length argv))
    (match (car argv)
        [(or "cat" "show")
         (show-task id)]
        [(or "desc" "set-desc" "sd") (block
         (assert!! (= 1 argc))
         (define desc (prompt-editor (or (task-desc t) "")))
         (task-set-desc! t (if (zero? (string-length desc)) #f desc)))]
        [(or "name" "rename" "title" "retitle" "n" "t") (block
         (assert!! (< 1 argc))
         (define name (string-join (cdr argv)))
         (task-set-title! t name))]
        [(or "block" "b")
         (assert!! (= 1 argc))
         (assert!! (not (task-done? t)))
         (task-block! t)]
        [(or "ready" "r")
         (assert!! (= 1 argc))
         (assert!! (not (task-ready? t)))
         (task-ready! t)]
        [(or "assign" "ass" "a") (block
         (assert!! (<= 1 argc 2))
         (assert!! (task-ready? t))
         (assert!! (not (task-done? t)))
         (define u (if (= 1 argc) (user-id (get-user-me)) (second argv)))
         (get-user u) ; for assert
         (task-assign! t u))]
        [(or "done" "d" "finish" "fin" "f")
         (assert!! (= 1 argc))
         (assert!! (task-assigned? t))
         (assert!! (not (task-done? t)))
         (task-done! t)]
        [(or "eval" "e") (block
         (assert!! (<= 2 argc 4))
         (define cur-user (get-user-me))
         (user-set-interest! cur-user id (second argv))
         (when (<= 3 argc) (user-set-priority! cur-user id (third argv)))
         (when (<= 4 argc) (user-set-needs-refinement! cur-user id (fourth argv))))]
        [(or "help" "?")
         (define helpmsg (list
            "cat show               show the task"
            "sd  set-desc           rewrite the description"
            "n   rename \"<name>\""
            "b   block              set the task as blocked"
            "r   ready"
            "a   assign [<user-id>] assign to user, or to self by default"
            "d   done"
            "e   eval <i> <p> <nr>  interest, priority, needs refinement?"
            "q   quit"
            "?   help               display this message"))
         (for-each displayln helpmsg)]
        [(or "quit" "q")
         (set! continue? #f)])
    continue?)

(define (edit-task id)
    (define t (get-task id))
    (define continue? #t)
    (retry-until-success (block
        (prompt (format "ed ~a@~a~a" (user-id (get-user-me)) (dmpath->string) id))
        (eof-barrier)
        (define argv (read-line-tokens))
        (set! continue? (and (handle-edit-task id argv) continue?))))
    (when continue? (edit-task id)))

(define (task->summaryrow t)
    (list
        (~a (task-id t))
        (~a (task-title t))
        (cond
            [(not (task-ready? t))    "b   "]
            [(not (task-assigned? t)) " r  "]
            [(not (task-done? t))     "  a "]
            [#t                       "   d"])
        (cond
            [(and (task-ready? t) (not (task-assigned? t))) 
             (define p (get-task-priority t))
             (if p (~r (get-user-task-assignment-index (get-user-me) t) #:precision 0) "-")]
            [(and (task-assigned? t) (not (task-done? t)))
             (task-assigned-to t)]
            [#t ""])))

(define (list-tasks tasks)
    (define tab (map task->summaryrow tasks))
    (print-table tab
        '(1 20 4 0)
        '(20 60 4 1000)
        '(2 1 1 2)
        '(right left left left)))
                           
(define (eval-task-loop t)
    (define id (task-id t))
    (define res
        (retry-until-success 
            (prompt (format "eval ~a@~a~a" (user-id (get-user-me)) (dmpath->string) id))
            (eof-barrier)
            (define argv (read-line-tokens))
            (match (car argv)
                [(or "q" "quit") #f]
                [(or "s" "skip") #t]
                [(or "?" "help")
                 (define helpmsg (list
                    "    <i> <p> <nr>       evaluate the task and continue"
                    "s   skip               skip this task"
                    "q   quit               exit evaluation"
                    "?   help               display this message"))
                 (for-each displayln helpmsg)
                 'loop]
                [_ (handle-edit-task id (cons "e" argv)) #t])))
    (if (eq? 'loop res) (eval-task-loop t) res))
(define (eval-tasks remaining)
    (unless (empty? remaining) (block
        (define t (car remaining))
        (define id (task-id t))
        (printf "~a left." (length remaining))
        (show-task id)
        (when (eval-task-loop t) (eval-tasks (cdr remaining))))))
        

    












