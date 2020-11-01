#lang racket

(require tests/drracket/private/drracket-test-util
         racket/runtime-path
         racket/gui/base
         quickscript/library
         framework
         rackunit
         "base.rkt")

;; TODO: Add test for when the racket version is changed,
;; or Racket BC / Racket CS for user-scripts
;; TODO: Test disable script in library?

;; Make sure the scripts subdirectory is registered in Quickscript
;; so that the scripts appear in the menu.
(define-runtime-path script-dir "scripts")
(add-third-party-script-directory! script-dir)
(define lib (load))
(exclude! lib script-dir "unbound-id.rkt")
(include! lib script-dir "unbound-id-not-skipped.rkt")
(save! lib)


;; Scripts compiled with an old version of racket BC or CS should be recompiled
;; and not raise an exception.
(make-directory* (build-path script-dir "compiled"))
(copy-file (build-path script-dir "compiled-old" "test-compile_rkt--7.7.0.901.zo")
           (build-path script-dir "compiled" "test-compile_rkt.zo")
           #t)
(copy-file (build-path script-dir "compiled-old" "test-compile-cs_rkt--7.8.0.6_cs.zo")
           (build-path script-dir "compiled" "test-compile-cs_rkt.zo")
           #t)

(define prefs `(,tools-prefs))

(fire-up-drracket-and-run-tests
 #:prefs prefs
 (λ ()
   ;; The script "unbound-id-not-skipped" raises an exception on startup.
   ;; Click ok on the message box and deactivate the script (so as to avoid
   ;; further exceptions. TODO: deactivate it in Quickscript).
   (define exn-dialog (wait-for-dialog/frame "Quickscript caught an exception"))
   (check-not-false exn-dialog)
   ; deactivate early
   (exclude! lib script-dir "unbound-id-not-skipped.rkt")
   (save! lib)
   (send exn-dialog focus)
   #;(display-window-hierarchy exn-dialog)
   (define bt-ok (find-widget exn-dialog (λ (x) (is-a? x button%))))
   (check-not-false bt-ok)
   (send bt-ok command (make-object control-event% 'button))
   ; Now that the script 
   #;(displayln "passed")

   (define drr (wait-for-drracket-frame))
   (define (get-canvas) (send drr get-definitions-canvas))
   (define (get-text) (send drr get-definitions-text))

   ;; Call scripts on text editor
   (queue-callback/res (λ () (send (get-canvas) focus)))
   (run-script "string-insert")
   (wait-for-drracket-frame)
   (run-script "string-reverse")
   (wait-for-drracket-frame)
   (queue-callback/res
    (λ ()
      (check string-suffix? ; suffix in case of a pre-inserted #lang line
             (send (get-text) get-text)
             "rotide eht si sihT")))
   (wait-for-drracket-frame)

   ;; output-to new-tab
   (run-script "output-to-new-tab")
   (wait-for-drracket-frame)
   (queue-callback/res
    (λ ()
      (check-equal? (send drr get-tab-count)
                    2)
      (check string-suffix?
             (send (get-text) get-text)
             "in new tab")))
   (wait-for-drracket-frame)

   ;; Ask drracket to open file.
   (run-script "open-me")
   (wait-for-drracket-frame)
   (queue-callback/res
    (λ ()
      (check-equal? (send drr get-tab-count)
                    3)
      (check-equal? (send drr get-tab-filename 2)
                    "open-me.rkt")))
   (wait-for-drracket-frame)

   (run-script "close-tab")
   (queue-callback/res
    (λ () (check-equal? (send drr get-tab-count)
                        2)))
   (wait-for-drracket-frame)

   ;; Persistent.
   (queue-callback/res
    (λ () (send drr create-new-tab)))
   (wait-for-drracket-frame)
   (run-script "show-counter")
   (queue-callback/res
    (λ () (check string-suffix? (send (get-text) get-text) "\n0")))
   (wait-for-drracket-frame)
   (run-script "increase-counter")
   (run-script "increase-counter")
   (run-script "increase-counter")
   (queue-callback/res
    (λ () (check string-suffix? (send (get-text) get-text) "\n3")))
   (wait-for-drracket-frame)
   (run-script "show-counter")
   (queue-callback/res
    (λ () (check string-suffix? (send (get-text) get-text) "\n0")))
   (wait-for-drracket-frame)
   ;; Unload persistent scripts.
   (manage-scripts "Unload persistent scripts")
   (run-script "increase-counter")
   (queue-callback/res
    (λ () (check string-suffix? (send (get-text) get-text) "\n1")))
   (wait-for-drracket-frame)

   ;; Create new script.   
   (new-script-and-run drr)
   

   (remove-third-party-script-directory! script-dir)
   #;(displayln "All done.")
   #t))

