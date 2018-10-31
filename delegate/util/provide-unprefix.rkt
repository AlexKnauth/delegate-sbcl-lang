#lang racket/base

(provide unprefix-out)

(require racket/provide
         racket/provide-syntax
         (for-syntax racket/base
                     syntax/parse))
(module+ test
  (require (for-syntax rackunit)))

(define-provide-syntax unprefix-out
  (syntax-parser
    [(_ prefix:id provide-spec)
     #:with pre (symbol->string (syntax-e #'prefix))
     #'(filtered-out (string-unprefix 'pre) provide-spec)]))

(begin-for-syntax
  ;; string-unprefix : String -> [String -> [Maybe String]]
  (define ((string-unprefix pre) str)
    (define pre-n (string-length pre))
    (and (<= pre-n (string-length str))
         (string=? pre (substring str 0 pre-n))
         (substring str pre-n))))

(module+ test
  (begin-for-syntax
    (check-equal? ((string-unprefix "s:") "s:abacaba") "abacaba")
    (check-equal? ((string-unprefix "s:") "s;abacaba") #false)
    (check-equal? ((string-unprefix "s:") "can:t") #false)
    (check-equal? ((string-unprefix "ca") "can:t") "n:t")))

