#lang racket

(require tests/drracket/private/drracket-test-util
         racket/runtime-path
         quickscript/library
         framework
         rackunit)

;; Make sure the scripts subdirectory is registered in Quickscript
;; so that the scripts appear in the menu.
(define-runtime-path script-dir "scripts")
(add-third-party-script-directory! script-dir)

(define (run-script name)
  (test:menu-select "Scripts" "Tests" name))

(define prefs
  `((plt:framework-pref:drracket:tools-configuration
     (
      ;; Check if quickscript-git exists, and if so disable quickscript
      ,@(if (collection-file-path "main.rkt" "quickscript-git" #:fail (λ _ #f))
          '[(((lib "quickscript") ("tool.rkt")) skip)]
          '[])
      ;; Deactivate some scripts for faster loading
      (((lib "frtime" "tool") "frtime-tool.rkt") skip)
      (((lib "deinprogramm") "DMdA/private/DMdA-langs.rkt") skip)
      (((lib "deinprogramm") "sdp/private/sdp-langs.rkt") skip)
      (((lib "algol60") ("tool.rkt")) skip)
      ))))


(fire-up-drracket-and-run-tests
 #:prefs prefs
 (λ ()
   (define drr (wait-for-drracket-frame))
   (define (get-canvas) (send drr get-definitions-canvas))
   (define (get-text) (send drr get-definitions-text))
   
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

   (run-script "open-me")
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

   ;; test persistent
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
   ;; unload persistent scripts
   (test:menu-select "Scripts" "Manage scripts" "Unload persistent scripts")
   (run-script "increase-counter")
   (queue-callback/res
    (λ () (check string-suffix? (send (get-text) get-text) "\n1")))
   (wait-for-drracket-frame)
   
   (displayln "All done.")
   #t))
