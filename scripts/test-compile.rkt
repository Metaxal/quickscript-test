#lang racket/base

(require quickscript)

(define-script test-compile.rkt
  #:label "test-compile"
  #:menu-path ("Tests")
  (λ (selection)
    #f))
