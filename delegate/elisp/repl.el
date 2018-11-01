
;; repl.el script from Kyle Burton on Stack Overflow:
;; https://stackoverflow.com/questions/145175/how-to-invoke-an-interactive-elisp-interpreter-in-emacs/145212#145212

(require 'cl)


(defun read-expression ()
  (condition-case
      err
      (read-string "> ")
    (error
     (message "Error reading '%s'" form)
     (message (format "%s" err)))))

(defun read-expression-from-string (str)
  (condition-case
      err
      (read-from-string str)
    (error
     (message "Error parsing '%s'" str)
     (message (format "%s" err))
     nil)))


(defun repl ()
  (loop for expr = (read-string "> ") then (read-expression)
        do
        (let ((form (car (read-expression-from-string expr))))
          (condition-case
              err
              (message " => %s" (eval form))
            (error
             (message "Error evaluating '%s'" form)
             (message (format "%s" err)))))))

(repl)
