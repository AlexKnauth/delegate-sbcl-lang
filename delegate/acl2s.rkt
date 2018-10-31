#lang racket/base

(provide (rename-out [module-begin #%module-begin]
                     [top #%top]
                     [app #%app]
                     [closure-syntax syntax])
         #%datum
         quote
         quasiquote
         unquote
         t nil
         defun defunc
         lambda
         let let*
         if cond
         + - * /
         atom consp cons car cdr first rest
         )

(require racket/list
         racket/system
         racket/port
         syntax/parse/define
         "util/which.rkt"
         (for-syntax racket/base
                     syntax/parse))

;; -----------------------------------------------

(define-simple-macro (module-begin form:expr ...)
  (#%module-begin
   (void (λ () form ... (void)))
   (delegate-acl2s/repl '(form ...))))

;; -----------------------------------------------

;; For Scoping / Check-Syntax-Arrow purposes only

(begin-for-syntax
  (define expr-scope
    (make-interned-syntax-introducer 'expr))
  (define function-scope
    (make-interned-syntax-introducer 'function))
  (define all-scopes
    (list expr-scope function-scope))
  ;; -----
  (define (no-ns stx)
    (for/fold ([stx stx]) ([s (in-list all-scopes)])
      (s stx 'remove)))
  (define (intro-expr stx)
    (expr-scope (no-ns stx) 'add))
  (define (intro-function stx)
    (function-scope (no-ns stx) 'add)))

(define-simple-macro (top . x:id)
  'x)

(define-simple-macro (app f:id a:expr ...)
  #:with f* (intro-function #'f)
  (#%app f* a ...))

(define-simple-macro (closure-syntax f)
  #:with f* (intro-function #'f)
  f*)

(define-simple-macro (lambda (x:id ...) body:expr ...+)
  #:with (x* ...) (intro-expr #'(x ...))
  #:with (body* ...) (intro-expr #'(body ...))
  (λ (x* ...) body* ...))

(define t 't)
(define nil 'nil)
(define consp 'consp)
(define atom 'atom)

(define-simple-macro (defun f:id (x:id ...) body:expr ...+)
  #:with f* (intro-function #'f)
  #:with (x* ...) (intro-expr #'(x ...))
  #:with (body* ...) (intro-expr #'(body ...))
  (define (f* x* ...) body* ...))

(define-syntax-parser defunc
  #:datum-literals [:input-contract :output-contract]
  [(_ f:id (x:id ...)
      :input-contract ic:expr
      :output-contract oc:expr
      body:expr ...+)
   #:with f* (intro-function #'f)
   #:with (x* ...) (intro-expr #'(x ...))
   #:with (ic* oc* body* ...) (intro-expr #'(ic oc body ...))
   #'(define (f* x* ...)
       (void (λ () ic* oc*))
       body* ...)])

;; -----------------------------------------------

;; Delagating to ACL2s

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


