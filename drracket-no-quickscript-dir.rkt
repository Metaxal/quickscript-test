#lang racket

(require tests/drracket/private/drracket-test-util
         quickscript/base
         rackunit
         "base.rkt")

;; Test if DrRacket starts correctly when the quickscript directory does not initially exist,
;; which is the setup of a first install.

(remove-quickscript-dir-until-exit!)

(define prefs `(,tools-prefs))

;; Open DrRacket normally
(fire-up-drracket-and-run-tests
 #:prefs prefs
 (λ ()
   (define drr (wait-for-drracket-frame))
   (new-script-and-run drr)
   #t))
