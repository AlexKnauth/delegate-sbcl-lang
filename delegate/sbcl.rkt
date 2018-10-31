#lang racket/base

(provide (rename-out [module-begin #%module-begin]))

(require racket/system
         racket/port
         racket/file
         syntax/parse/define)

(define-simple-macro (module-begin form:expr ...)
  (#%module-begin
   (delegate-sbcl/repl '(form ...))))

(define sbcl
  #;(find-executable-path "sbcl")
  #;(with-output-to-string (λ () (system "which sbcl")))
  #;(with-output-to-string (λ () (system "bash -c 'which sbcl'")))
  "/usr/local/bin/sbcl")

;; delegate-sbcl/repl : [Listof S-Expr] -> Void
(define (delegate-sbcl/repl forms)
  (define tmp (make-temporary-file))
  (define tmp-out (open-output-file tmp #:exists 'replace))
  (write-forms forms tmp-out)
  (close-output-port tmp-out)
  (define command
    (format "~v --end-runtime-options --load ~v"
            sbcl
            (path->string tmp)))
  (void (system command)))

;; write-forms : [Listof S-Expr] OutputPort -> Void
(define (write-forms forms out)
  (for ([form (in-list forms)])
    (writeln form out)))


