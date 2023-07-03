#lang racket

(require
    contract-profile
    profile)

(require
    "../src/config.rkt"
    "../src/domain.rkt"
    "../src/model.rkt"
    "../src/repl/task-list.rkt"
    "../src/repl/utils.rkt")

(startup)
(domain/login (current-domain) (get-user-by-name "hans"))

(when #f
(contract-profile
    #:report-space-efficient? #t
    (parameterize ([current-output-port (open-output-nowhere)])
        (for ((i 10000))
            (eprintf " ~a\r" i)
            ((cmdentry-handler repl-summary) 1 '(":"))
            ((cmdentry-handler repl-list-tasks) 1 '("l")))))
)

(when #t
(profile
    #:order 'self
    #:use-errortrace? #t
    (parameterize ([current-output-port (open-output-nowhere)])
        (for ((i 10000))
            (eprintf " ~a\r" i)
            ((cmdentry-handler repl-summary) 1 '(":"))
            ((cmdentry-handler repl-list-tasks) 1 '("l")))))
)
