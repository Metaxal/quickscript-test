#lang racket

(require tests/drracket/private/drracket-test-util
         racket/runtime-path
         racket/gui/base
         (prefix-in lib: quickscript/library)
         framework
         rackunit
         syntax/parse/define
         "base.rkt")

;;; Try with
;;;   raco test drracket-hooks.rkt
;;; but be sure to try also with
;;;   raco test --drdr  drracket-hooks.rkt
;;; In particular, this ensures that a fresh user is created with no script.

;;; To debug:
;;; export PLTSTDERR="debug@quickscript debug@qstest" && racket -l quickscript-test/drracket
;;; Or
;;; export PLTSTDERR=debug@qstest && racket -l quickscript-test/drracket

;; Make sure the scripts subdirectory is registered in Quickscript
;; so that the scripts appear in the menu.
(define-runtime-path script-dir "scripts/hooks")
(lib:add-third-party-script-directory! script-dir)

(define all-hooks-filename "all-hooks.rkt")
(define all-hooks-path (build-path script-dir all-hooks-filename))
;; Used only to load a file
(define string-reverse-filename "string-reverse.rkt")
(define string-reverse-script-path (build-path script-dir 'up string-reverse-filename))

(define-logger qstest)

(define prefs `(,tools-prefs))

(define lg-rec (make-log-receiver (current-logger) 'debug 'qs-test))

(define (prx msg)
  (define l (string-split msg "ARG"))
  (pregexp (string-append* (add-between (map regexp-quote l) "[^\n]*"))))

(define prx-on-tab-change
  (prx "qs-test: hook: on-tab-change\n[#:tab-from ARG\n[#:tab-to ARG]\n"))

(define (check-hook-messages . msgs)
  (for ([msg  (in-list msgs)])
    (define v (sync/timeout 1 lg-rec))
    (define rx (if (regexp? msg) msg (pregexp (regexp-quote msg))))
    (match v
      [(vector 'debug msg2 #f 'qs-test)
       (if (regexp-match rx msg2)
         (printf "hook check passed: ~v\n" v)
         (eprintf "Fail check.\n Hook message: ~v\n Expected: ~v\n" msg2 msg))]
      [else
       (eprintf "Bad hook message.\n Got: ~v\n Expected: ~v\n" v msg)])))

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

   ;; Load a file in the first untouched empty tab
   (handler:edit-file string-reverse-script-path)
   (check-hook-messages
    (prx (format "qs-test: hook: after-load-file\n[#:file ARG~a]\n[#:in-new-tab? #f]\n"
                 string-reverse-filename)))

   ;; Create empty new tab
   (create-new-tab)
   (check-hook-messages
    prx-on-tab-change
    (prx "qs-test: hook: after-create-new-tab\n"))

   ;; Open file in new tab
   (create-new-tab all-hooks-path)
   (check-hook-messages
    prx-on-tab-change
    (prx (format "qs-test: hook: after-load-file\n[#:file ARG~a]\n[#:in-new-tab? #t]\n"
                 all-hooks-filename)))

   ;; Close current tab
   ;; Currently raises a Gtk error which happens whenever a tab is closed. Not Racket's fault it
   ;; seems
   #;
   (begin
     (close-current-tab)
     (check-hook-messages
      (prx "qs-test: hook: on-tab-close\n")
      prx-on-tab-change))

   ;; TODO: save file hook

   ;; Clean up
   (lib:remove-third-party-script-directory! script-dir)

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
   (check-hook-messages "qs-test: hook: on-close\n")

   ;; Get all messages
   (let loop ()
     (define msg (sync/timeout 1 lg-rec))
     (when msg
       (eprintf "Missed hook message: ~v\n" msg)
       (loop)))

   #t))

