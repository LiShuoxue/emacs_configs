(require 'python)

(defun my-python-number-highlighting ()
  (font-lock-add-keywords
   nil
   `(
     ;; Scientific
     ("\\_<[0-9]+\\(?:\\.[0-9]*\\)?[eE][+-]?[0-9]+\\_>" . 'font-lock-number-face)
     ;; Complex
     ("\\_<\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\(?:[eE][+-]?[0-9]+\\)?[jJ]\\_>" . 'font-lock-number-face)
     ;; Regular
     ("\\_<[0-9]+\\(?:\\.[0-9]*\\)?\\_>" . 'font-lock-number-face)
     )
   )
  )


(defun my-python-highlight-operators ()
  (font-lock-add-keywords
   nil
   '(("\\([=+*/%@&|^<>!-]=?\\|\\*\\*\\|//\\|<<\\|>>\\|[@~]\\)"
      1 'font-lock-operator-face)
     )))


(defun summarize-flymake-errors ()
  "Summarize all Flymake diagnostics in the current buffer."
  (interactive)
  (let ((diagnostics (flymake-diagnostics (point-min) (point-max))))
    (if diagnostics
        (with-output-to-temp-buffer "*Flymake Summary*"
          (dolist (diag diagnostics)
            (let ((type (flymake-diagnostic-type diag))
                  (text (flymake-diagnostic-text diag))
                  (line (flymake-diagnostic-beg-line diag)))
              (princ (format "Line %d: [%s] %s\n" line type text)))))
      (message "No Flymake issues found."))))


(setq python-shell-interpreter "ipython")
      ;; python-shell-interpreter-args "-i --simple-prompt")

(add-hook 'python-mode-hook #'my-python-number-highlighting)
(add-hook 'python-mode-hook #'my-python-highlight-operators)
