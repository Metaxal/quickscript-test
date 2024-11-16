#lang racket

(require tests/drracket/private/drracket-test-util
         racket/runtime-path
         racket/gui/base
         (prefix-in lib: quickscript/library)
         framework/test
         rackunit
         quickscript/base
         "base.rkt")

;;; To run in parallel (important for focus checking):
;;; $ parallel racket -t drracket.rkt -- -q -q -q

;;; To debug:
;;; export PLTSTDERR="debug@quickscript debug@qstest" && racket -l quickscript-test/drracket
;;; Or
;;; export PLTSTDERR=debug@qstest && racket -l quickscript-test/drracket

;; TODO: Add test for when the racket version is changed,
;; or Racket BC / Racket CS for user-scripts
;; TODO: Test disable script in library?

;; NOTICE: Use `queue-callback/res` instead of `queue-callback` because for the latter
;; errors may not be (re)raised leading to silent bugs.

;; Make sure the user script library is included, otherwise some tests will fail, like
;; when creating a new script.
(lib:add-third-party-script-directory! user-script-dir)

;; Make sure the scripts subdirectory is registered in Quickscript
;; so that the scripts appear in the menu.
(define-runtime-path script-dir "scripts")
(lib:add-third-party-script-directory! script-dir)
(define lib (lib:load))
(lib:exclude! lib script-dir "unbound-id.rkt")
(lib:include! lib script-dir "unbound-id-not-skipped.rkt")
(lib:save! lib)

(define-logger qstest)

(define test-compiled-old-zo (build-path script-dir "compiled-old" "test-compile_rkt--7.7.0.901.zo"))
(define test-compiled-old-dep (build-path script-dir "compiled-old" "test-compile_rkt--7.7.0.901.dep"))
(define test-compiled-zo (build-path script-dir "compiled" "test-compile_rkt.zo"))
(define test-compiled-dep (build-path script-dir "compiled" "test-compile_rkt.dep"))

(define test-compiled-cs-old-zo (build-path script-dir "compiled-old" "test-compile-cs_rkt--7.8.0.6_cs.zo"))
(define test-compiled-cs-old-dep (build-path script-dir "compiled-old" "test-compile-cs_rkt--7.8.0.6_cs.dep"))
(define test-compiled-cs-zo (build-path script-dir "compiled" "test-compile-cs_rkt.zo"))
(define test-compiled-cs-dep (build-path script-dir "compiled" "test-compile-cs_rkt.dep"))


;; Scripts compiled with an old version of racket BC or CS should be recompiled
;; and not raise an exception.
(make-directory* (build-path script-dir "compiled"))
(copy-file test-compiled-old-zo
           test-compiled-zo
           #t)
(copy-file test-compiled-old-dep
           test-compiled-dep
           #t)
(copy-file test-compiled-cs-old-zo
           test-compiled-cs-zo
           #t)
(copy-file test-compiled-cs-old-dep
           test-compiled-cs-dep
           #t)

(define prefs `(,tools-prefs))

(fire-up-drracket-and-run-tests
 #:prefs prefs
 (λ ()
   ;; The script "unbound-id-not-skipped" raises an exception on startup.
   ;; Click ok on the message box and deactivate the script (so as to avoid
   ;; further exceptions. TODO: deactivate it in Quickscript).
   (log-qstest-info "Before exception dialog")
   (define exn-dialog (wait-for-dialog/frame "Quickscript: Error during compilation"))
   (check-not-false exn-dialog)
   (log-qstest-info "Exception dialog found")
   ; deactivate early
   (lib:exclude! lib script-dir "unbound-id-not-skipped.rkt")
   (lib:save! lib)
   (send exn-dialog focus)
   (log-qstest-info "After exception dialog")
   #;(display-window-hierarchy exn-dialog)
   (define bt-ok (find-widget exn-dialog (λ (x) (is-a? x button%))))
   (check-not-false bt-ok)
   (send bt-ok command (make-object control-event% 'button))

   #; ; There will not be an error while loading the menus as the script is deactivated
   (begin
     (define exn-dialog2 (wait-for-dialog/frame "Quickscript: Errors while loading script properties"))
     (check-not-false exn-dialog2)
     (define bt-ok2 (find-widget exn-dialog2 (λ (x) (is-a? x button%))))
     (check-not-false bt-ok2)
     (send bt-ok2 command (make-object control-event% 'button)))

   (define drr (wait-for-drracket-frame))
   (define (get-defs-canvas) (send drr get-definitions-canvas))
   (define (get-text) (send drr get-definitions-text))
   (define (move-focus-to-defs)
     (queue-callback/res (λ () (send (get-defs-canvas) focus))))
   (define (create-new-tab)
     (define n (send drr get-tab-count))
     (queue-callback/res (λ () (send drr create-new-tab)))
     ;; Make sure the tab is created.
     (poll-until (λ () (= (+ n 1) (send drr get-tab-count)))))

   ;; Call scripts on text editor
   (move-focus-to-defs)
   (run-script "string-insert")
   (move-focus-to-defs)
   (run-script "string-reverse")
   (move-focus-to-defs)
   (queue-callback/res
    (λ ()
      (check string-suffix? ; suffix in case of a pre-inserted #lang line
             (send (get-text) get-text)
             "rotide eht si sihT")))

   ;; output-to new-tab
   (run-script "output-to-new-tab")
   (move-focus-to-defs)
   (queue-callback/res
    (λ ()
      (check-equal? (send drr get-tab-count)
                    2)
      (check string-suffix?
             (send (get-text) get-text)
             "in new tab")))

   ;; Simulate a recompilation of a script from a different racket version
   ;; after DrRacket has started.
   (copy-file test-compiled-old-dep
              test-compiled-dep
              #t)
   ;;This does nothing but should not raise a compilation error exception.
   (run-script "test-compile")
   (move-focus-to-defs)

   ;; Ask drracket to open file.
   (run-script "open-me")
   (move-focus-to-defs)
   (queue-callback/res
    (λ ()
      (check-equal? (send drr get-tab-count)
                    3)
      (check-equal? (send drr get-tab-filename 2)
                    "open-me.rkt")))

   ;; Currently raises a Gtk error which happens whenever a tab is closed. Not Racket's fault it
   ;; seems
   #;
   (begin
     (run-script "close-tab")
     (ensure-defs-has-focus)
     (queue-callback/res
      (λ ()  (check-equal? (send drr get-tab-count)
                           2))))

   ;; Persistent.
   (create-new-tab)
   (move-focus-to-defs)
   (run-script "show-counter")
   (move-focus-to-defs)
   (queue-callback/res
    (λ () (check string-suffix? (send (get-text) get-text) "\n0")))
   (run-script "increase-counter")
   (run-script "increase-counter")
   (run-script "increase-counter")
   (move-focus-to-defs)
   (queue-callback/res
    (λ () (check string-suffix? (send (get-text) get-text) "\n3")))
   (run-script "show-counter")
   (move-focus-to-defs)
   (queue-callback/res
    (λ () (check string-suffix? (send (get-text) get-text) "\n0")))
   ;; Unload persistent scripts.
   (manage-scripts "Stop persistent scripts")
   (run-script "increase-counter")
   (move-focus-to-defs)
   (queue-callback/res
    (λ () (check string-suffix? (send (get-text) get-text) "\n1")))

   ;; Create new script.
   (new-script-and-run drr)

   (lib:remove-third-party-script-directory! script-dir)
   #;(displayln "All done.")
   #t))

