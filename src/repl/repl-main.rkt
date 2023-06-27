#lang racket/base

(require
    racket/function racket/list
    racket/match    racket/block
    "../utils.rkt"
    "../model.rkt"
    "../task.rkt"
    "../domain.rkt"
    "../user.rkt"
    "utils.rkt"
    "task-list.rkt"
    "task-misc.rkt"
    "repl-task-edit.rkt"
    "repl-eval.rkt"
)
(provide repl)

(define new-user 0)

(define (handle-repl argc argv)
    (repl-menu-switch argc argv (car argv) (list
        repl-summary
        (cmdentry '("e" "eval")
                  '(("e" "eval" "enter evaluation mode"))
                  (thunk*
                     (assert!! (= 1 argc))
                     (define tasks-to-eval
                        (query-tasks (sort-by < task-id)
                                     (filter-by (curry user-needs-eval-task? (me)))))
                     (repl-eval tasks-to-eval)))
        (cmdentry '("n" "new" "new-task")
                  '(("n" "new-task \"<name>\"" "create a new task"))
                  (thunk*
                     (assert!! (= 2 argc))
                     (new-task (second argv))))
        (cmdentry '("ed" "edit" "edit-task")
                  '(("ed" "edit-task <id>" "enter editing mode for task <id>")
                    (""   "          <id> <cmd>" "    run <cmd> in editing mode for the task"))
                  (thunk*
                     (assert!! (<= 2 argc))
                     (if (= argc 2) 
                         (repl-edit (second argv))
                         (handle-edit-task (second argv) (cddr argv)))))
        (cmdentry '("cat" "show" "show-task" "view" "view-task")
                  '(("cat" "view-task <id>" "view details of a task"))
                  (thunk*
                     (assert!! (= 2 argc))
                     (show-task (second argv))))
        repl-list-tasks
        cmdentry-spacer
        (cmdentry '("reload")
                  '(("" "reload" "recover state in this domain from last commit to file"))
                  (thunk* (load)))
        (cmdentry '("commit")
                  '(("" "commit" "commit changes in this domain to file"))
                  (thunk* (commit)))
        (cmdentry '("q" "quit")
                  '(("q" "quit" "quit, committing changes in all domains"))
                  (thunk* (save-and-exit)))
        (cmdentry '("q!" "exit")
                  '(("q!" "exit" "quit without committing any changes"))
                  (thunk* (exit)))
        cmdentry-spacer
        (cmdentry '("cd" "change-domain")
                  '(("cd" "change-domain <path>" "change to another domain")
                    (""   "    /..."             "    from the root domain")
                    (""   "    ~ or <empty>"     "    to the home domain"))
                  (thunk*
                     (assert!! (<= argc 2))
                     (define path 
                        (match argv
                            [`(,_ "/") "~:"]
                            [`(,_ ,str) (string-append str ":")]
                            [_ "~:"]))
                     (select-domain (itempath-dmpath (string->itempath path)))))
        (cmdentry '("ld" "list-domains")
                  '(("ld" "list-domains" "see all domains"))
                  (thunk*         
                     (for ([dmf (in-domain domain-tree-root)])
                        (displayln (dmpath->string (domain-frame-path dmf))))))
        (cmdentry-desc '(("" "<path>:<cmd>" "run <cmd> in domain at <path>")))
        cmdentry-spacer
        (cmdentry '("u" "user" "new-user")
                  '(("u" "new-user <name>" "create a new user"))
                  (thunk* (block
                     (assert!! (= 2 argc))
                     (define name (second argv))
                     (assert!! (not (hash-has-key? (domain-id->user (current-domain)) name)))
                     (register-user (make-user name))
                     (printf "User ~a registered.\n" name))))
        (cmdentry '("swap-user")
                  '(("" "swap-user" "login to a different user"))
                  (thunk* login)))))

(define (repl)
    (retry-until-success (block
        (if (get-user-me)
            (prompt (format "~a@~a" (user-id (me)) (dmpath->string)))
            (prompt (format "~a" (dmpath->string))))
        (eof-barrier)
        (define argv (read-line-tokens))
        (define argc (length argv))
        (assert!! (< 0 argc))
        (match-define (itempath cmd-dom cmd) (string->itempath (car argv)))
        (if (empty? cmd-dom)
            (handle-repl argc (cons cmd (cdr argv)))
            (parameterize ([current-domain-frame (resolve-domain-frame cmd-dom)])
                (handle-repl argc (cons cmd (cdr argv)))))))
    (repl))


