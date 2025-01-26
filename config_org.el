;; Customization for org-mode in emacs



(defun my/org-refresh-inline-images ()
  "Refresh inline images in the current Org buffer."
  (when (derived-mode-p 'org-mode)
    (org-display-inline-images)))

(add-hook 'org-mode-hook 'org-display-inline-images)
(add-hook 'after-save-hook 'my/org-refresh-inline-images)

(setq org-image-actual-width 300)  ;; Set maximum width in pixels

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(org-link-set-parameters
 "function-help"
 :follow (lambda (func-name)
           "Jump to the help page of a function."
           (describe-function (intern func-name)))
 :export (lambda (func-name description format)
           (cond ((eq format 'html) (format "<a href=\"%s\">%s</a>" func-name (or description func-name)))
                 ((eq format 'latex) (format "\\href{%s}{%s}" func-name (or description func-name))))))

