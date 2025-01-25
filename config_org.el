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

