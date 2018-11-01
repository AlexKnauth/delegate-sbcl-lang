#lang racket/base

(provide write-whitespace-toward)

(module+ test
  (require rackunit))

;; ---------------------------------------------------------

;; write-whitespace-toward : Nat Nat Nat InputPort -> Void
;; Writes whitespace such that `in` (assumed to be fresh),
;; gets to line `ln`, column `col`, and position `pos`.
(module+ test
  (define-check (check-write-whitespace-toward ln col pos str)
    (parameterize ([port-count-lines-enabled #true])
      (define out (open-output-string))
      (write-whitespace-toward ln col pos out)
      (define-values [ln* col* pos*] (port-next-location out))
      (check-equal? ln* ln "line")
      (check-equal? col* col "column")
      (check-equal? pos* pos "position")
      (check-equal? (get-output-string out) str)))

  (check-write-whitespace-toward 1 0 1 "")
  (check-write-whitespace-toward 1 17 18 (make-string 17 #\space))
  (check-write-whitespace-toward 2 17 19
                                 (string-append "\n"
                                                (make-string 17 #\space)))
  (check-write-whitespace-toward 2 17 24
                                 (string-append (make-string 5 #\space) "\n"
                                                (make-string 17 #\space)))
  (check-write-whitespace-toward 3 17 20
                                 (string-append "\n"
                                                "\n"
                                                (make-string 17 #\space)))
  (check-write-whitespace-toward 3 17 29
                                 (string-append (make-string 9 #\space) "\n"
                                                "\n"
                                                (make-string 17 #\space)))
  (check-write-whitespace-toward 5 17 29
                                 (string-append (make-string 7 #\space) "\n"
                                                "\n"
                                                "\n"
                                                "\n"
                                                (make-string 17 #\space)))
  )

(define (write-whitespace-toward ln col pos out)
  (unless (exact-positive-integer? ln)
    (error 'write-whitespace-toward "line should be a positive integer"))
  (unless (exact-nonnegative-integer? col)
    (error 'write-whitespace-toward "column should be a natural number"))
  (unless (exact-positive-integer? pos)
    (error 'write-whitespace-toward "position should be a positive integer"))
  (cond
    [(= 1 ln)
     (unless (= pos (add1 col))
       (error 'write-whitespace-toward
              "position ~v and column ~v are inconsistent for line ~v"
              pos col ln))
     (write-string (make-string col #\space) out)
     (void)]
    [else
     (define spaces-on-last-line col)
     (define newlines (sub1 ln))
     (define chars-total (sub1 pos))
     (define spaces-on-first-line (- chars-total spaces-on-last-line newlines))
     (unless (exact-nonnegative-integer? spaces-on-first-line)
       (error 'write-whitespace-toward
              "position ~v, line ~v, and column ~v are inconsistent"
              pos ln col))
     (write-string (make-string spaces-on-first-line #\space) out)
     (write-string (make-string newlines #\newline) out)
     (write-string (make-string spaces-on-last-line #\space) out)
     (void)]))

