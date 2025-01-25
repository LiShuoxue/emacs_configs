;; BASIC CUSTOMIZATION
;; --------------------------------------


(defun open_init_config ()
  "Open the emacs configuration folder in the current buffer."
  (interactive)
  (find-file "~/.emacs.d/emacs_configs/")
  )


(defun confirm-suspend-frame ()
  "Ask for confirmation before suspending Emacs."
  (interactive)
  (when (yes-or-no-p "Really suspend Emacs? ")
    (suspend-frame)))


(defun scale_buffer_height (scale)
  (let ((desired-height (floor (* scale (frame-height)))))
    (shrink-window (- (window-total-height) desired-height))
    )
  )


(defun scale_buffer_width (scale)
  (let ((desired-width (floor (* scale (frame-width)))))
    (shrink-window-horizontally (- (window-total-width) desired-width))
    )
)


(defun init_layout ()
  (split-window-right)
  (scale_buffer_width 0.20)
  (find-file ".")
  (other-window 1)
  (select-window (split-window-below))
  (scale_buffer_height 0.30)
  (let(
	(display-buffer-overriding-action '(display-buffer-same-window) )
	)
    (shell)
    )
  (select-window (previous-window))
  )


(global-set-key (kbd "C-c i") 'open_init_config)
(global-set-key (kbd "C-z") 'confirm-suspend-frame) ; Confirm before suspending Emacs
(setq confirm-kill-emacs #'yes-or-no-p)             ; Confirm before exiting Emacs
(setq inhibit-startup-message t)   ;; hide the startup message
(setq neo-smart-open t)

;; enable line numbers globally
(if (>= emacs-major-version 29)
    (global-display-line-numbers-mode t)
  (global-linum-mode t))

(setq linum-format "%5d\s\s")  ;; format line number spacing

(add-hook 'prog-mode-hook #'show-paren-mode)  ; Highlight matching parentheses
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'emacs-startup-hook #'init_layout)
(global-auto-revert-mode t)
(delete-selection-mode t)                    ; Replace selection when typing
(setq inhibit-startup-message t)          ; Hide the startup message
(setq make-backup-files nil)
