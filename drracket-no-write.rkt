#lang racket

#|

This test checks:

  - if quickscript attempts to write anything to disk
    - on startup
    - on compilation
    - ...

also try:

   raco test -l tests/drracket/no-write-and-frame-leak

Caveats:
  - It may not use the right version of quickscript, i.e., quickscript-git
  - It may raise errors about duplicate labels in the Script menu (but this may be okay?)
  - It may complain about recompiling scripts (but this may be okay?)

|#

(require tests/drracket/private/drracket-test-util
         rackunit
         "base.rkt")

;; ERROR: the move-back is called while the security guard is still on,
;; raising an exception
(remove-quickscript-dir-until-exit!)
;; TODO: add an uncompiled script to trigger compilation?
;; (not sure if that's the intent of the test)

(define write-ok #f)

(with-handlers (#;[(λ (e) (displayln e) (eq? e nice-exit))
                 ;; Make sure that the program exits *outside* of the security guard,
                 ;; so as to move the quickscript directory back to its normal place.
                 (λ (e) (exit 0))])
  (parameterize ([current-security-guard
                  (make-security-guard
                   (current-security-guard)
                   (λ (who pth what)
                     (unless write-ok
                       (when (member 'write what)
                         (error who "Writing to the file system is not allowed"))
                       (when (member 'delete what)
                         (error who "Deleting files is not allowed"))))
                   void
                   void)])
    (fire-up-drracket-and-run-tests 
     #:prefs `([plt:framework-pref:drracket:online-compilation-default-on #f]
               [plt:framework-pref:framework:autosaving-on? #f]
               ,tools-prefs)
     (λ ()
       (define drr (wait-for-drracket-frame))
       (check-equal? #t #t)
       
       ; Must be last: Write/delete is now ok.
       (set! write-ok #t)))))
