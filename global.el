
;; BASIC CUSTOMIZATION
;; --------------------------------------


; customized key-bindings

(defun open_init_config ()
  "
  Open the emacs configuration folder in the current buffer.
"
  (interactive)
  (find-file "~/.emacs.d/emacs_configs/")
  )


(global-set-key (kbd "C-c i") 'open_init_config)

(setq inhibit-startup-message t)   ;; hide the startup message

;; enable line numbers globally
(if (>= emacs-major-version 29)
    (global-display-line-numbers-mode t)
  (global-linum-mode t))

(setq linum-format "%5d\s\s")  ;; format line number spacing
;; Allow hash to be entered  

(setq confirm-kill-emacs #'yes-or-no-p)       ; Confirm before exiting Emacs
(add-hook 'prog-mode-hook #'show-paren-mode)  ; Highlight matching parentheses
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-auto-revert-mode t)
(delete-selection-mode t)                    ; Replace selection when typing
(setq inhibit-startup-message t)          ; Hide the startup message
(setq make-backup-files nil)
