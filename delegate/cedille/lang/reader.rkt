#lang racket

(require syntax/strip-context)
 
(provide (rename-out [literal-read read]
                     [literal-read-syntax read-syntax]))
 
(define (literal-read in)
  (syntax->datum
   (literal-read-syntax #f in)))
 
(define (literal-read-syntax src in)
  (port-count-lines! in)
  (define-values [ln col pos] (port-next-location in))
  (with-syntax ([src src]
                [ln ln]
                [col col]
                [pos pos]
                [str (port->string in)])
    (strip-context
     #'(module anything delegate/cedille
         src
         ln
         col
         pos
         str))))
