#lang racket

(require tests/drracket/private/drracket-test-util
         racket/runtime-path
         racket/gui/base
         quickscript/library
         framework
         rackunit)

;; TODO: Add test for when the racket version is changed,
;; or Racket BC / Racket CS for user-scripts
;; TODO: Test disable script in library?

(define-syntax (debug-read-line stx)
  (syntax-case stx ()
      [(_)
       #`(begin (displayln (format "Line: ~a\n" #,(syntax-line stx)))
         (read-line))]))

;; Make sure the scripts subdirectory is registered in Quickscript
;; so that the scripts appear in the menu.
(define-runtime-path script-dir "scripts")
(add-third-party-script-directory! script-dir)

(define (run-script name)
  (test:menu-select "Scripts" "Tests" name))
(define (manage-scripts name)
  (test:menu-select "Scripts" "Manage scripts" name))

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
   (begin
     ; random name.
     (define new-script-name (format "script-~a" (current-milliseconds)))
     (manage-scripts "New script…")
     ;; For this to work, get-text-from-user (tool.rkt) must use
     ;; #:dialog-mixin frame:focus-table-mixin.
     (define script-name-msgbox (wait-for-new-frame drr))
     (for ([c (in-string new-script-name)])
       (test:keystroke c))
     (test:button-push "OK")
     (wait-for-new-frame script-name-msgbox) ; should be drr
     ; Edit script.
     (queue-callback/res
      (λ ()
        (send (get-canvas) focus)
        (define text (get-text))
        (define end (send text last-position))
        (send text set-position 0 end)
        (send text clear)
        (define new-script-text
          #<<EOS
#lang racket/base

(require quickscript)

(define-script test-new+clipboard
  #:label "new+clipboard"
  #:output-to clipboard
  #:menu-path ("Tests")
  (λ (selection #:file f)
    (path->string f)))
EOS
          )
        (send text insert new-script-text)))
     (test:menu-select "File" "Save Definitions")
     (run-script "new+clipboard")
     (define filestring
       (queue-callback/res
        (λ ()
          (send the-clipboard get-clipboard-string 0))))
     (check-true (file-exists? filestring))
     (delete-file filestring))
   

   
   (displayln "All done.")
   #t))
