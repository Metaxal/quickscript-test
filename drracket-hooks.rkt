#lang racket

(require tests/drracket/private/drracket-test-util
         racket/runtime-path
         racket/gui/base
         (prefix-in lib: quickscript/library)
         framework
         rackunit
         syntax/parse/define
         "base.rkt")

;;; To debug:
;;; export PLTSTDERR="debug@quickscript debug@qstest" && racket -l quickscript-test/drracket
;;; Or
;;; export PLTSTDERR=debug@qstest && racket -l quickscript-test/drracket

;; TODO: Add test for when the racket version is changed,
;; or Racket BC / Racket CS for user-scripts
;; TODO: Test disable script in library?

;; Make sure the scripts subdirectory is registered in Quickscript
;; so that the scripts appear in the menu.
(define-runtime-path script-dir "scripts/hooks")
(lib:add-third-party-script-directory! script-dir)
(define lib (lib:load))

(define-logger qstest)

(define prefs `(,tools-prefs))

(define lg-rec (make-log-receiver (current-logger) 'debug 'qs-test))

(define (check-hook-messages . msgs)
  (for ([msg  (in-list msgs)])
    (define v (sync/timeout 1 lg-rec))
    (define rx (if (regexp? msg) msg (pregexp (regexp-quote msg))))
    (match v
      [(vector 'debug msg2 #f 'qs-test)
       (unless (regexp-match rx msg2)
         (eprintf "Fail check.\n Hook message: ~v\n Expected: ~v\n" msg2 msg))]
      [else
       (error "Bad hook message" v)])
    #;(writeln v)
    ;;check-match does not display good error messages here
    #;(check-match v
                 (vector 'debug (pregexp (if (regexp? msg) msg (regexp-quote msg))) #f 'qs-test))))

(fire-up-drracket-and-run-tests
 #:prefs prefs
 (λ ()
   (define drr (wait-for-drracket-frame))
   (define (get-defs-canvas) (send drr get-definitions-canvas))
   (define (get-text) (send drr get-definitions-text))
   (define (ensure-defs-has-focus)
     (queue-callback/res (λ () (send (get-defs-canvas) focus)))
     ;; This should not be necessary since queue-callback/res is synchronous :/
     (poll-until (lambda () (send (get-defs-canvas) has-focus?))))
   (define (create-new-tab [filename #f])
     (define n (send drr get-tab-count))
     (queue-callback/res
      (λ () (send drr create-new-tab filename)))
     ;; Make sure the tab is created.
     (poll-until (λ () (= (+ n 1) (send drr get-tab-count)))))

   (define (close-current-tab)
     (define n (send drr get-tab-count))
     (queue-callback/res
      (λ () (send drr close-current-tab)))
     ;; Make sure the tab is created.
     (poll-until (λ () (= (- n 1) (send drr get-tab-count)))))


   (check-hook-messages
    "qs-test: hook: on-startup\n"
    "qs-test: hook: after-create-new-drracket-frame\n[#:show? #f]\n")

   (define (prx msg)
     (define l (string-split msg "ARG"))
     (pregexp (string-append* (add-between (map regexp-quote l) "[^\n]*"))))

   (define prx-on-tab-change
     (prx "qs-test: hook: on-tab-change\n[#:tab-from ARG\n[#:tab-to ARG]\n"))

   ;; TODO: Check hook events when loading a file in the first untouched empty tab
   
   (create-new-tab)
   (check-hook-messages
    ;; WARNING: need to use a regexp maybe
    prx-on-tab-change
    (prx "qs-test: hook: after-create-new-tab\n[#:tab ARG]\n"))
   
   (create-new-tab (build-path script-dir "all-hooks.rkt"))
   (check-hook-messages
    ;; WARNING: order is reversed compared to create-new-tab, because don't use the same methods?
    ;; or because of async?
    (prx "qs-test: hook: after-load-file\n[#:hook-editor ARG]\n[#:file #f]\n")
    prx-on-tab-change
    ;; NOTICE: This hook is better because it is called AFTER the tab change,
    ;; but unfortunately it's not called when loading in the current tab for the very first file load.
    ;; TODO: unify the two cases!
    (prx "qs-test: hook: after-open-file-in-new-tab\n[#:file ARG]\n[#:filename ARG]\n"))

   ;; Clean up
   (lib:remove-third-party-script-directory! script-dir)
   #;(displayln "All done.")

   ;; Make sure hooks are removed after Reload Menu without the hooks
   (manage-scripts "Reload menu")
   (create-new-tab) ; This should NOT generate a message from "all-hooks.rkt"
   

   ;; For some reason, if we don't wait we get a message when trying to close:
   ;; "The program is still running, are you sure you want to quit?"
   (sleep/yield 1)

   ;; Closing. By default, drr asks for user input to validate the closing in a dialog%,
   ;; so we wait for the dialog to show up before clicking on the Quit button.
   (thread (λ ()
             (define exn-dialog (wait-for-dialog/frame "Warning"))
             (define bt-ok (find-widget exn-dialog (λ (x) (and (is-a? x button%)
                                                               (equal? (send x get-label)
                                                                       "Quit")))))
             (send bt-ok command (make-object control-event% 'button))))
   (send drr close)

   ;; WARNING: This message is sometimes not caught!
   (check-hook-messages "qs-test: hook: on-close\n")

   ;; Get all messages
   (let loop ()
     (define msg (sync/timeout 1 lg-rec))
     (when msg
       (eprintf "Missed hook message: ~v\n" msg)
       (loop)))
   
   #t))

