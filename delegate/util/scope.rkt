#lang racket/base

(provide (rename-out [scope-#%top #%top]
                     [scope-#%app #%app]
                     [scope-closure syntax])
         #%datum
         ; ----------
         ; top level forms
         defun defunc
         (rename-out [begin progn])
         ; ----------
         ; expression forms
         quote
         quasiquote
         unquote
         t nil
         let let*
         if cond
         ; ----------
         ; function-expression forms
         lambda
         ; ----------
         ; functions
         funcall apply
         + - * /
         atom consp cons car cdr
         list first rest second
         )

(require racket/list
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse))

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

(define-simple-macro (scope-#%top . x:id)
  'x)

(define-simple-macro (scope-#%app f:id a:expr ...)
  #:with f* (intro-function #'f)
  (#%app f* a ...))

(define-simple-macro (scope-closure f)
  #:with f* (intro-function #'f)
  f*)

(define-simple-macro (lambda (x:id ...) body:expr ...+)
  #:with (x* ...) (intro-expr #'(x ...))
  #:with (body* ...) (intro-expr #'(body ...))
  (λ (x* ...) body* ...))

(define (funcall f . as)
  (apply f as))

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

