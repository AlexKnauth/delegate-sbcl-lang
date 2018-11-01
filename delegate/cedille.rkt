#lang racket/base

(provide (rename-out [module-begin #%module-begin]))

(require racket/list
         racket/file
         racket/system
         racket/port
         syntax/parse/define
         "util/which.rkt"
         "util/provide-unprefix.rkt"
         (prefix-in acl2s: "util/scope.rkt")
         (for-syntax racket/base
                     syntax/parse))

;; -----------------------------------------------

(define-simple-macro (module-begin s:str)
  (#%module-begin
   (delegate-cedille 's)))

;; -----------------------------------------------

;; Delagating to ACL2s

(define cedille
  (which "cedille"))

;; delegate-cedille : String -> Void
(define (delegate-cedille s)
  (define tmp (make-temporary-file))
  (define tmp-out (open-output-file tmp #:exists 'replace))
  (write-string s tmp-out)
  (close-output-port tmp-out)
  (define command
    (format "~v" cedille))
  (define in
    (open-input-string
     (format "check§~a\n" (path->string tmp))))
  (void (parameterize ([current-input-port in])
          (system command))))

