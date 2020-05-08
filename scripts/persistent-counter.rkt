#lang racket/base
(require quickscript)

(define count 0)

(define-script increase-counter
  #:label "increase-counter"
  #:menu-path ("Tests")
  #:persistent
  (λ (selection) 
    (set! count (+ count 1))
    (format "\n~a" count)))

(define-script show-counter/non-persistent
  #:label "show-counter"
  #:menu-path ("Tests")
  (λ (selection)
    (format "\n~a" count)))
