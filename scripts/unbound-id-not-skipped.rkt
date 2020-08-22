#lang racket/base

(require quickscript)

;; This script is NOT disabled,
;; an error message box should be displayed, 
;; and DrRacket then start normally.
(define-script unbound-id
  #:label "unbound-id"
  #:menu-path ("Tests")
  (λ (selection)
    some-unbound-id))
