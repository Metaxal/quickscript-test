#lang racket/base

(require quickscript
         racket/class)

(define-script close-tab
  #:label "close-tab"
  #:menu-path ("Tests")
  (λ (selection #:frame fr)
    (send fr close-current-tab)))
