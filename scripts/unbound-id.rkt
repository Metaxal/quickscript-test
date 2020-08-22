#lang racket/base

(require quickscript)

;; This script is disabled and should not cause DrRacket to crash
(define-script string-reverse
  #:label "string-reverse"
  #:menu-path ("Tests")
  (λ (selection)
    abracadabra
    (apply string (reverse (string->list selection)))))
