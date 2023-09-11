#lang racket/base

(require racket/gui/base
         racket/class
         quickscript
         syntax/parse/define
         (for-syntax racket/base
                     quickscript))

(define-syntax-parse-rule (define-hook* id (~seq keyword:keyword kwid) ...)
  ;; INFO: add a quote `'` before the next paren and Run to debug
  (define-hook id
      (λ ({~@ keyword kwid} ...)
        (define str
          (string-append (format "hook: ~a\n" 'id)
                         (format "[~a ~a]\n" 'keyword kwid)
                         ...))
        #;(writeln str)
        (log-message (current-logger) 'debug 'qs-test str))))

(define-hook* on-startup)
(define-hook* on-close)
(define-hook* after-create-new-drracket-frame #:show? show?)
(define-hook* after-load-file #:file f #:in-new-tab? in-new-tab?)
(define-hook* on-save-file #:save-filename sf #:file f)
(define-hook* after-save-file #:file f)
(define-hook* on-tab-close #:tab tab)
(define-hook* on-tab-change #:tab-from ft #:tab-to tt)
(define-hook* after-create-new-tab)

;; An error should be shown for this one:
;; This is now done at the syntax level.
#;(define-hook* undefined #:show? show?)
