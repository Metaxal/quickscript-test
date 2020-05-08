#lang info
(define collection "quickscript-test")
(define deps '("drracket-test"
               "gui-lib"
               "quickscript"
               "rackunit-lib"
               "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/quickscript-test.scrbl" ())))
(define pkg-desc "Tests for Quickscript")
(define version "0.1")
#;(define pkg-authors '(laurent))
