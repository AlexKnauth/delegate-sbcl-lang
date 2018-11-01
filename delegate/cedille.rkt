#lang racket/base

(provide (rename-out [module-begin #%module-begin]))

(require racket/file
         racket/list
         racket/match
         racket/system
         syntax/parse/define
         json
         "cedille/check.rkt"
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

;; Delagating to Cedille

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
  (define cedille-json
    (cedille-check tmp))
  ;; handle the JSON value
  (handle-cedille-json cedille-json))

;; -----------------------------------------------

;; A CedilleJson is a Json object:
;;   {
;;     spans: SpansJson
;;   }
;; A SpansJson is a Json array of SpanJson
;; A SpanJson is a Json array:
;;   [String, Pos, Pos, ???]

;; handle-cedille-json : CedilleJson -> Any
(define (handle-cedille-json cj)
  (match cj
    [(hash-table ['spans ssj])
     (map handle-span-json ssj)]))

;; handle-span-json : SpanJson -> Any
(define (handle-span-json sj)
  (match sj
    [(list desc start end _)
     (list desc start (- end start))]))

