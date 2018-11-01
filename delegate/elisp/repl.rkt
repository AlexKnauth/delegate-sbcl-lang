#lang racket/base

(provide elisp-repl)

(require racket/runtime-path
         racket/system)

(define-runtime-path repl.el "repl.el")

;; only handles single-line forms
(define (elisp-repl)
  (define command
    (format "emacs -batch -l ~v"
            (path->string repl.el)))
  (system command))
