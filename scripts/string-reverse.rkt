#lang racket/base

(require quickscript)

(define-script string-reverse
  #:label "string-reverse"
  #:menu-path ("Tests")
  (λ (selection #:other-keyword [y ""] . rst)
    (apply string-append
           (apply string (reverse (string->list selection)))
           ""
           rst)))
