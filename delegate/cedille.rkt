#lang racket/base

(provide (rename-out [module-begin #%module-begin]))

(require racket/list
         racket/file
         racket/system
         racket/port
         syntax/parse/define
         json
         "util/which.rkt"
         "util/write-whitespace-toward.rkt"
         (for-syntax racket/base
                     syntax/parse))
(module+ test
  (require rackunit))

;; -----------------------------------------------

(define-simple-macro (module-begin src ln:nat col:nat pos:nat s:str)
  (#%module-begin
   (delegate-cedille 'src 'ln 'col 'pos 's)))

;; -----------------------------------------------

;; Delagating to ACL2s

(define cedille
  (which "cedille"))

;; delegate-cedille : Any Nat Nat Nat String -> Void
(define (delegate-cedille src ln col pos s)
  (define tmp (make-temporary-file))
  (define tmp-out (open-output-file tmp #:exists 'replace))
  (write-whitespace-toward ln col pos tmp-out)
  (write-string s tmp-out)
  (close-output-port tmp-out)
  ;; ----------
  (define-values [my-in their-out] (make-pipe))
  (define command
    (format "~v" cedille))
  (define in
    (open-input-string
     (format "check§~a\n" (path->string tmp))))
  (void (parameterize ([current-input-port in]
                       [current-output-port their-out])
          (system command)))
  ;; get rid of lines that start with "progress: "
  (consume-progress-lines my-in)
  ;; read a JSON value
  (read-json my-in))

;; consume-progress-lines : InputPort -> Void
;; get rid of lines that start with "progress: "
(define (consume-progress-lines in)
  (define progress-start "progress: ")
  (define would-be-progress (peek-string (string-length progress-start) 0 in))
  (cond
    [(and (string? would-be-progress)
          (string=? would-be-progress progress-start))
     (read-line in)
     (consume-progress-lines in)]
    [else
     (void)]))

