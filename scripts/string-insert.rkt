#lang racket/base

(require quickscript
         racket/class)

(define-script string-insert
  #:label "string-insert"
  #:menu-path ("Tests")
  (λ (selection #:editor ed)
    (define str "This is the editor")
    (send ed insert str)
    (define pos (send ed get-start-position))
    (send ed set-position (- pos (string-length str)) pos)
    #f))
