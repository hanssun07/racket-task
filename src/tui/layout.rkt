#lang racket/base

;; homebrew implementation of pretty-expressive
;; to work with formatting and block components

(require
    "../utils/ann.rkt"
    "format.rkt"
)

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
