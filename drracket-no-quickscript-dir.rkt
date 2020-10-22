#lang racket

(require tests/drracket/private/drracket-test-util
         racket/runtime-path
         racket/gui/base
         quickscript/base
         quickscript/library
         ffi/unsafe/custodian
         framework
         rackunit)

(define-syntax (debug-read-line stx)
  (syntax-case stx ()
      [(_)
       #`(begin (displayln (format "Line: ~a\n" #,(syntax-line stx)))
         (read-line))]))

;; Test if DrRacket starts correctly when the quickscript directory does not initially exist,
;; which is the setup of a first install.
(define new-quickscript-dir #f)
(when (directory-exists? quickscript-dir)
  ;; Move the existing dir to a temp name
  (set! new-quickscript-dir
        (normalize-path ; to remove 'up
         (build-path quickscript-dir 'up (format "quickscript-temp-~a" (current-milliseconds)))))
  (rename-file-or-directory quickscript-dir new-quickscript-dir)
  ;; Make sure we rename the old directory to its original name when DrRacket exits.
  (register-custodian-shutdown
   new-quickscript-dir
   (λ (new-dir)
     (when new-dir
       (when (directory-exists? quickscript-dir)
         (delete-directory/files quickscript-dir))
       ;; move back from the temp name to the correct name
       (rename-file-or-directory new-dir quickscript-dir)))
   #:at-exit? #t)
  (void))

(define prefs
  `((plt:framework-pref:drracket:tools-configuration
     (
      ;; Check if quickscript-git exists, and if so disable quickscript.
      ,@(if (collection-file-path "main.rkt" "quickscript-git" #:fail (λ _ #f))
          '[(((lib "quickscript") ("tool.rkt")) skip)]
          '[])
      ;; Deactivate some scripts for faster loading.
      (((lib "frtime" "tool") "frtime-tool.rkt") skip)
      (((lib "deinprogramm") "DMdA/private/DMdA-langs.rkt") skip)
      (((lib "deinprogramm") "sdp/private/sdp-langs.rkt") skip)
      (((lib "algol60") ("tool.rkt")) skip)
      ))))

(fire-up-drracket-and-run-tests
 #:prefs prefs
 (λ ()
   (define drr (wait-for-drracket-frame))
   (check-equal? #t #t) ; just to say that one test has passed
   #t))
