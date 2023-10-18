#lang racket

(require tests/drracket/private/drracket-test-util
         quickscript/base
         (prefix-in lib: quickscript/library)
         rackunit
         "base.rkt")

;; Test if DrRacket starts correctly when the quickscript directory does not initially exist,
;; which is the setup of a first install.
;; DrDr should do that already though.

(remove-quickscript-dir-until-exit!)

;; Make sure the user script library is included, otherwise some tests will fail, like
;; when creating a new script.
(lib:add-third-party-script-directory! user-script-dir)

(define prefs `(,tools-prefs))

;; Open DrRacket normally
(fire-up-drracket-and-run-tests
 #:prefs prefs
 (λ ()
   (define drr (wait-for-drracket-frame))
   (new-script-and-run drr)
   #t))
