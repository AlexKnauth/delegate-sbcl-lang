#lang racket/base

(provide (rename-out [module-begin #%module-begin]))

(require racket/file
         racket/list
         racket/match
         racket/system
         syntax/parse/define
         json
         "cedille/check.rkt"
         "util/write-whitespace-toward.rkt"
         (for-syntax racket/base
                     #;racket/file
                     #;racket/match
                     syntax/parse
                     #;"cedille/check.rkt"
                     #;"util/write-whitespace-toward.rkt"))
(module+ test
  (require rackunit))

;; -----------------------------------------------

(define-simple-macro (module-begin src ln:nat col:nat pos:nat s:str)
  (#%module-begin
   ;(mouse-over-tooltips src ln col pos s)
   (delegate-cedille 'src 'ln 'col 'pos 's)))

;; -----------------------------------------------

;; Delagating to Cedille

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

;; Generating Mouse-Over Tooltips

#;(define-syntax-parser mouse-over-tooltips
  [(_ src* ln*:nat col*:nat pos*:nat s*:str)
   (define src (syntax->datum #'src*))
   (define ln (syntax-e #'ln*))
   (define col (syntax-e #'col*))
   (define pos (syntax-e #'pos*))
   (define s (syntax-e #'s*))
   (syntax-property
    #'(begin)
    'mouse-over-tooltips
    (generate-mouse-over-tooltips src ln col pos s))])

#;(begin-for-syntax
  ;; A MouseOverTooltip is a:
  ;;   (Vector
  ;;     ; A syntax object whose `syntax-source` field indicates which
  ;;     ; file the tooltip goes in
  ;;     Syntax
  ;;     ; The start position in the editor (zero-indexed)
  ;;     Nat
  ;;     ; The end position in the editor (zero-indexed)
  ;;     Nat
  ;;     ; The content of the tooltip, or a function to produce the content
  ;;     (U String (-> String)))

  ;; generate-mouse-over-tooltips :
  ;; Any Nat Nat Nat String -> [ConsTreeof MouseOverTooltip]
  (define (generate-mouse-over-tooltips src ln col pos s)
    (define tmp (make-temporary-file))
    (define tmp-out (open-output-file tmp #:exists 'replace))
    (write-whitespace-toward ln col pos tmp-out)
    (write-string s tmp-out)
    (close-output-port tmp-out)
    ;; ----------
    (define src-stx (datum->syntax #f #f (list src ln col pos 0)))
    (define cedille-json (cedille-check tmp))
    (define spans (hash-ref cedille-json 'spans))
    (for/list ([span (in-list spans)])
      (match span
        [(list desc start end _)
         (vector-immutable src-stx (sub1 start) (sub1 end) desc)]))))

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

