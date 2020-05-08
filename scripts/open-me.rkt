#lang racket/base

(require quickscript
         racket/class
         racket/path
         racket/runtime-path
         syntax/location)

(define-runtime-path here ".")
(define this-file
  (build-path here (file-name-from-path (quote-source-file))))

(define-script open-me
  #:label "open-me"
  #:menu-path ("Tests")
  (λ (selection #:frame fr)
    (send fr open-in-new-tab this-file)))
