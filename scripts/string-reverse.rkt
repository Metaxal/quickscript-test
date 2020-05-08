#lang racket/base

(require quickscript)

(define-script string-reverse
  #:label "string-reverse"
  #:menu-path ("Tests")
  (λ (selection)
    (apply string (reverse (string->list selection)))))
