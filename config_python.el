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


(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
