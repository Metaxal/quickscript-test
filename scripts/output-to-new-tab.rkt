#lang racket/base

(require quickscript
         racket/class)

(define-script output-to-new-tab
  #:label "output-to-new-tab"
  #:menu-path ("Tests")
  #:output-to new-tab
  (λ (selection #:editor ed)
    "in new tab"))
