#lang racket/base

(provide (rename-out [module-begin #%module-begin]))

(require racket/system
         racket/string
         racket/port
         racket/file
         syntax/parse/define
         "util/which.rkt")

(define-simple-macro (module-begin form:expr ...)
  (#%module-begin
   (delegate-acl2s/repl '(form ...))))

(define acl2s
  (which "acl2s"))

;; delegate-acl2s/repl : [Listof S-Expr] -> Void
(define (delegate-acl2s/repl forms)
  (define-values [tmp-in tmp-out] (make-pipe))
  (write-forms forms tmp-out)
  (close-output-port tmp-out)
  (define appended-in
    (input-port-append #true tmp-in (current-input-port)))
  (define command
    (format "~v" acl2s))
  (void (parameterize ([current-input-port appended-in])
          (system command))))

;; write-forms : [Listof S-Expr] OutputPort -> Void
(define (write-forms forms out)
  (for ([form (in-list forms)])
    (writeln form out)))

