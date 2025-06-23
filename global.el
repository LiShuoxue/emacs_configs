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
  (neotree)
  )


(global-set-key (kbd "C-z") 'confirm-suspend-frame) ; Confirm before suspending Emacs
(global-set-key (kbd "C-c i") 'open_init_config)
(global-set-key (kbd "C-c f") 'flymake-show-buffer-diagnostics)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c t") 'load-theme)
(global-set-key (kbd "C-c v") 'multi-vterm) ; Open multi-vterm
(global-set-key (kbd "C-c p f") 'python-pytest-file)

(setq confirm-kill-emacs #'yes-or-no-p)             ; Confirm before exiting Emacs
(setq inhibit-startup-message t)   ;; hide the startup message
(setq neo-smart-open t)

;; enable line numbers globally
(if (>= emacs-major-version 29) (global-display-line-numbers-mode t) (global-linum-mode t))

(setq linum-format "%5d\s\s")  ;; format line number spacing

(add-hook 'prog-mode-hook #'show-paren-mode)  ; Highlight matching parentheses
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'emacs-startup-hook #'init_layout)
(global-auto-revert-mode t)
(delete-selection-mode t)                    ; Replace selection when typing
(setq inhibit-startup-message t)          ; Hide the startup message
(setq make-backup-files nil)


(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs_configs/themes/")
(load-theme 'oriole t)

(setq term-terminal-parameter '(:color-mode . true))

;; Save the current layout 
(desktop-save-mode 1)

(require 'org-tempo)

;; (defconst +session-dir+ (expand-file-name "~/.emacs.d/session/"))

;; (require 'desktop)
;; (setq desktop-save t desktop-load-locked-desktop t desktop-path `(,+session-dir+) desktop-dirname +session-dir+ desktop-base-file-name ".desktop" desktop-base-lock-name ".desktop.lock") (desktop-save-mode t)

;; save history between sessions
;; (require 'savehist)
;; (setq history-length 100 savehist-file (concat +session-dir+ ".history")) (savehist-mode t)

;; save point between file visits
;; (require 'saveplace)
;; (setq save-place-limit 100 save-place-file (concat +session-dir+ ".point")) (setq-default save-place t)
; (require 'treesit)
; (treesit-available-p)  ;; t
; (setq treesit-extra-load-path '("/home/sli7/myenv/lib"))
