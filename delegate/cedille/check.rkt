#lang racket/base

(provide cedille-check)

(require racket/string
         racket/system
         json
         "../util/which.rkt")

;; -----------------------------------------------

;; Data Definitions

;; A CedilleJson is a Json object:
;;   {
;;     spans: SpansJson
;;   }
;; A SpansJson is a Json array of SpanJson
;; A SpanJson is a Json array:
;;   [String, Pos, Pos, ???]

;; -----------------------------------------------

;; Delegating to Cedille to Check a File

(define cedille
  (which "cedille"))

;; cedille-check : Path -> CedilleJson
;; Calls the cedille compiler with a "check" command
;; on the given file.
(define (cedille-check file-path)
  (define-values [my-in their-out] (make-pipe))
  (define command (format "~v" cedille))
  (define file-string (path->string file-path))
  (when (string-contains? file-string "\n")
    (error 'cedille-check "file path cannot contain a newline"))
  (when (string-contains? file-string "§")
    (error 'cedille-check "file path cannot contain a `§`"))
  (define in
    (open-input-string
     (format "check§~a\n" file-string)))
  (void (parameterize ([current-input-port in]
                       [current-output-port their-out])
          (system command)))
  (close-output-port their-out)
  ;; get rid of lines that start with "progress: "
  (consume-progress-lines my-in)
  ;; read a JSON value
  (read-json my-in))

;; consume-progress-lines : InputPort -> Void
;; Gets rid of lines that start with "progress: "
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

