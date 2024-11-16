#lang racket

(provide (all-defined-out))

(require tests/drracket/private/drracket-test-util
         racket/gui/base
         framework/test
         rackunit
         ffi/unsafe/custodian
         quickscript/base
         (for-syntax syntax/location))

;; TODO: Add test for when the racket version is changed,
;; or Racket BC / Racket CS for user-scripts

(define-syntax (dbg stx)
  (syntax-case stx ()
      [(_)
       #`(eprintf "Line: ~a source: ~a\n" #,(syntax-line stx) #,(syntax-source-file-name stx))]))

(define-syntax (debug-read-line stx)
  (syntax-case stx ()
      [(_)
       #`(begin (displayln (format "Line: ~a\n" #,(syntax-line stx)))
         (read-line))]))

(define (run-script name)
  (test:menu-select "Scripts" "Tests" name)
  (sleep/yield 0.5))

(define (manage-scripts name)
  (test:menu-select "Scripts" "Manage" name))

(define tools-prefs
  `(plt:framework-pref:drracket:tools-configuration
    (;; Deactivate some scripts for faster loading.
     (((lib "frtime" "tool") "frtime-tool.rkt") skip)
     (((lib "deinprogramm") "DMdA/private/DMdA-langs.rkt") skip)
     (((lib "deinprogramm") "sdp/private/sdp-langs.rkt") skip)
     (((lib "algol60") ("tool.rkt")) skip)
     )))

(define (wait-for-dialog/frame title)
  (define (wait-for-pred)
    (findf (λ (top)
             (equal? (send top get-label)
                     title))
           (get-top-level-windows)))
  (poll-until wait-for-pred))

(define (wait-for-pending-actions)
  (poll-until (λ () (= 0 (test:number-pending-actions)))))

;; Debugging tool.
(define (display-window-hierarchy wnd)
  (let loop ([wnd wnd] [depth 0])
    (printf "~a~a\n" (make-string (* 2 depth) #\space) wnd)
    (when (is-a? wnd area-container<%>)
      (for-each (λ (ch) (loop ch (+ depth 1))) (send wnd get-children)))))

(define (find-widget wnd pred)
  (let loop ([wnd wnd])
    (if (pred wnd)
      wnd
      (and (is-a? wnd area-container<%>)
           (ormap loop (send wnd get-children))))))

;; Moves the current quickscript directory to a different place until
;; the end of the test, then moves it back (using a custodian).
;; WARNING: Ctrl-C will prevent moving it back!
;; THIS IS RISKY (not used in main test suite for now)
(define old-quickscript-dir #f)
(define (remove-quickscript-dir-until-exit!)
  (when (directory-exists? quickscript-dir)
    ;; Move the existing dir to a temp name
    (set! old-quickscript-dir
          (normalize-path ; to remove 'up
           (build-path quickscript-dir 'up (format "quickscript-temp-~a" (current-milliseconds)))))
    (rename-file-or-directory quickscript-dir old-quickscript-dir)
    ;; Make sure we rename the old directory to its original name when DrRacket exits.
    (register-custodian-shutdown
     old-quickscript-dir
     (λ (new-dir)
       (when new-dir
         (unless (directory-exists? new-dir)
           (error (format (string-append
                           "quickscript backup directory should exist but doesn't: ~a.\n"
                           "Can't restore directory")
                          new-dir)))
         (when (directory-exists? quickscript-dir)
           (delete-directory/files quickscript-dir))
         ;; move back from the temp name to the correct name
         (rename-file-or-directory new-dir quickscript-dir)))
     #:at-exit? #t)
    (void)))

;; Creates a new script via the menus, run it, and check the result is correct
(define (new-script-and-run drr)
  (define (get-canvas) (send drr get-definitions-canvas))
  (define (get-text) (send drr get-definitions-text))
  ; random name.
  (define new-script-name (format "script-~a-~a" (current-milliseconds) (random (expt 2 31))))
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
     ;; NOTICE: A typo in the code below will be completely silent,
     ;; and the script will just not appear in the menu, leading
     ;; to a `run-script` failing to find the menu item.
     (define new-script-text
       #<<EOS
#lang racket/base

(require quickscript)

(define-script test-abcdef
  #:label "abcdef"
  #:output-to new-tab
  #:menu-path ("Tests")
  (λ (selection #:file f)
    "abcdef"))
EOS
       )
     (send text insert new-script-text)))
  ;; NOTICE: Make sure file running this script adds `user-script-dir` to the
  ;; library with:
  #;(lib:add-third-party-script-directory! user-script-dir)
  ;; early in the file, because `raco test --drdr` creates a new directory.
  (define script-file (build-path user-script-dir (string-append new-script-name ".rkt")))
  (check-pred file-exists? script-file)
  (test:menu-select "File" "Save Definitions")
  (manage-scripts "Reload menu") ; WARNING: It is unknown when this will finish!
  (run-script "abcdef")
  (queue-callback/res
   (λ ()
     (check string-suffix?
            (send (get-text) get-text)
            "abcdef")))
  (delete-file script-file))
