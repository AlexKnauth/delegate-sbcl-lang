#lang racket/base

(require "repl.rkt")
(module+ test
  (require rackunit))

;; -----------------------------------------------

;; make-elisp-evaluator : -> [S-Expr -> S-Expr]
(define (make-elisp-evaluator)
  (define-values [my-in their-out] (make-pipe))
  (define-values [their-in my-out] (make-pipe))
  (thread
   (λ () (parameterize ([current-input-port their-in]
                        [current-error-port their-out])
           (elisp-repl))))
  (define (evaluator expr)
    (read-expect '> my-in)
    (writeln expr my-out)
    (read-expect '=> my-in)
    (read my-in))
  evaluator)

(define (read-expect sym in)
  (define act (read in))
  (unless (equal? sym act)
    (eprintf "the repl is behaving badly, got: ~v" act)))

;; -----------------------------------------------

(module+ test
  (define ev (make-elisp-evaluator))
  (ev '(defun f (x)
         (+ x 1)))
  (check-equal? (ev '(f 5)) 6))
