#lang racket/base

;; homebrew implementation of pretty-expressive
;; to work with formatting and block components

(require
    "../utils/ann.rkt"
    "../utils.rkt"
    "format.rkt"
    racket/match
    megaparsack megaparsack/text data/monad data/applicative
)

#|
; for upward-result measures and downard-constrained bounds
(struct measure (mx my Mx My))

(struct ui ())
(struct ui:hlist  ui (children))
(struct ui:vlist  ui (children))
(struct ui:hspace ui (target min max))
(struct ui:vspace ui (target min max))

; ex. for table cells
(struct ui:bound  ui (measure))
(struct ui:overflow ui (child))

(struct ui:label ui (text))
(struct ui:para  ui (text))
(struct ui:sp    ui ())
(struct ui:nbsp  ui ())
(struct ui:lnbr  ui ())

(struct ui:alt ui (kinds))

; ex. for aligning : in keybinds
(struct ui:aligntok ui (tok))
|#

(struct ui ())
(struct ui:haligntok    ui ())
(struct ui:hspace       ui (width))
(struct ui:hfill        ui ())
(struct ui:valigntok    ui ())
(struct ui:vspace       ui (height))
(struct ui:vfill        ui ())
(struct ui:text-raw     ui (str) #:transparent)
(struct ui:text-break   ui () #:transparent)
(struct ui:alts         ui (a b) #:transparent)
(struct ui:append       ui (a b) #:transparent)
(struct ui:empty        ui () #:transparent)
(struct ui:with-cost    ui (penalty contents) #:transparent)

(struct ui-cost:rel-width (prop min max))

(define (ui:list . parts) (foldr ui:append (ui:empty) parts))
(define (ui:text-para str)
    (struct breakable (left right orig cost type))
    (struct hardbreak ())
    (define (breakable-family/p chs side
             #:cost [cost #f] #:type [type #f]) (do
        (ch <- (char-in/p chs))
        (pure (breakable (if (eq? side 'right) (string ch) "")
                         (if (eq? side 'left)  (string ch) "")
                         (string ch) cost type))))
    (define avoid-cost (ui-cost:rel-width 1/10 1 +inf.0))
    (define breakable/p (or/p
        (breakable-family/p " \t"       'subsume #:type 'space)
        (do (ch <- (char-in/p "\r\n\v"))  (pure (hardbreak)))
        (breakable-family/p "([{"       'left    #:type 'brac-left)
        (breakable-family/p ")]}"       'right   #:type 'brac-right)
        (breakable-family/p ".,:;!?"    'right   #:type 'punct)
        (breakable-family/p "\\/-_=+"   'right   #:cost avoid-cost)))
    (define word-char/p (char-not-in/p " \r\n\t\v.,:;!?()[]{}\\/-+=_"))
    (define word/p (do
        (chars <- (many+/p word-char/p))
        (pure (list->string chars))))
    (define parse/p (many/p (or/p word/p breakable/p)))
    (define toks (parse-result! (parse-string parse/p str)))
    (define (tok-merge a b) (match* (a b)
        [[(breakable la ra _a c ta) (breakable lb rb _b _ tb)]
         (if (and ta (equal? ta tb))
             (list (breakable (string-append la lb) (string-append ra rb) (string-append _a _b) c ta))
             (list a b))]
        [[(? string?) (? string?)]
         (list (string-append a b))]
        [[_ _] (list a b)]))
    (define toks-san (list-inner-merge tok-merge toks))
    ; TODO: memoization pass to dedup
    (define (merge-to-ui a b) (match* (a b)
        [[(? string?) (? ui?)]     (list (ui:append (ui:text-raw a) b))]
        [[(breakable l r s c _) (? ui?)]
         (let ([ui:break (ui:list (ui:text-raw l) (ui:text-break) (ui:text-raw r))])
             (list (ui:append (ui:alts s (if c (ui:with-cost c ui:break) ui:break))
                              b)))
         #;(let ([ui:break (ui:list (ui:text-raw l) (ui:text-break) (ui:text-raw r) b)])
             (list (ui:alts (ui:append s b)
                      (if c (ui:with-cost c ui:break) ui:break))))]
        [[(hardbreak) (? ui?)]     (list (ui:append a b))]))
    (list-inner-merge merge-to-ui (append toks-san (list (ui:empty))))
)

(require racket/pretty)
(pretty-print (ui:text-para "
hello world! test...
"))
#;(pretty-print (ui:text-para "
implement tables with aligmnt markers + margin elems + fill elems
-> allows inner tables spanning cells
-> align-in-cell by marker + margin + text + fill in correct ordering
-> reflow indents using two alignment markers on column left
-> balance h/v space usage by scoring empty space in bounding box
   defined by alignment markers
   -> structural-bounded-recursive dp to resolve
   -> avoid orphans/widows with empty-space penalties
   -> avoid overwide text with width threshold penalties
   -> balance hyphenation-breaks with modest penalty (~30% of a line)
-> spec on high-level components => spec on inline components
                                    (space + text + format)
   -> high-level components as alternatives of lower-level components


test...!?) ((abc)(...)))(()(((())))
"))
 

(define goal-a "
8 tasks in progress.
  work:22 manual: 2.2.3 outline value of caution                       rs- 32 alice
       31 manual: 7.2 structure out workflows                          rs- 16 alice
   task:0 feat: graph work                                             rs- 48 alice
       69 model: factor eval logic to eligible tasks,                  rs- 27 alice
                 then filter by ...
       72 ui+code: build infrastructure for output formatting          rs- 48 alice
       73 ui+code: commandline reader, parser a la shell,              rs- 48 alice 
                   racket/cmdline ...
       74 ui: rework q/q!/commit semantics: q should fail if dirty     rs- 18 alice
              ins...
          ...                   ; should look for balance between h/v space usage
30 tasks assigned.              ; in particular, balanced row filling across columns
  work:17 manual: 2.1 write media analysis                             r-- 32 alice
          ...                   ; maybe minimizing empty-space in a box?
No tasks awaiting evaluation.
39 tasks pending.
   work:5 research: project cybersyn                                   r-- 32
          ...
")
(define goal-b "
Logged in as  :alice,
          main:alice,        ; alignment markers + hfill elements
     main/work:alice,
     main/task:alice,
     peek-work:alice,
peek-work/alice:alice,
 peek-work/bob:bob,
peek-work/ryan:ryan
")
(define goal-c "
       summary                     get a summary of assigned, pending tasks
  e    eval                        enter evaluation mode
  n    new-task \"<name>\"           create a new task
  ed   edit-task <id>              enter editing mode for task <id>
                 <id> <cmd>            run <cmd> in editing mode for the task
                 (<id> ...) <cmd>      run for all <id>s   ; need gutter/margin per aligment marker
  cat  view-task <id>              view details of a task
  ls   list [opts ...]             list tasks...        ; need tables spanning cells of another table
        -a all                         repl is all pending and in-progress tasks
           ready blocked               require or exclude ready tasks
           assigned unassigned         require or exclude assigned tasks
           done not-done               require or exclude done tasks
           mine                        require assigned to me
           <number>                    maximum to show
        -t by-id                       order by ascending id
        -p by-priority                 order by descending priority score
                                     options trigger left-to-right

       reload                      recover state in this domain from       ; need to reflow in cells
                                       last commit to file                 ; with particular alignment
       commit                      commit changes in this domain to file
  q    quit                        quit, committing changes in all domains
  q!   exit                        quit without committing any changes

  cd   change-domain <path>        change to another domain
           /...                        from the root domain
           ~ or <empty>                to the home domain
  ld   list-domains                see all domains
       <path>:<cmd>                run <cmd> in domain at <path>

  u    new-user <name>             create a new user
       swap-user                   login to a different user

  ?    help                        view this entry
")
(define goal-d "
Task 35: Lorem ipsum

Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Fusce vitae suscipit elit. Donec iaculis placerat ante,         ; avoid widow words in sentences
et pretium dolor lacinia sed. Suspendisse id risus vel dolor
cursus tincidunt a vel est. Aliquam sed sem a diam              ; avoid orphaning words in sentences
mollis interdum. Donec convallis euismod augue, ut placerat
tellus vulputate a. In et hendrerit enim, sed sodales nibh.
In porta ornare leo et accumsan. Maecenas eu
pellentesque purus.

Ready 2023-06-28.
Assigned to alice.

Priority 3 (#3)
Your evaluations:
  3/4 interest
  3/4 priority
  0   needs refinement?
")

