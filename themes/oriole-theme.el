
(deftheme oriole
  "A minimal custom light theme.")

(let ((class '((class color)))

      (light-grey "#bbbbbb")
      (fg "#68174e")
      (bg "#effaec")
      (cursor "#114514")
      (highlight "#e4e4e4")
      (indentation-face "#c5ebba")
      (sidebar-active-fg "#204c8b")
      (sidebar-active-bg "#c5ebba")
      (region "#f5f7d3")
      (comment "#888888")
      (keyword "#2665a9")
      (decleration "#ba2fa7")
      (number "#5767d2")
      (builtin "#005f87")
      (string "#2f6a21")
      (type "#6138e8")
      (variable "#870000")
      (operator "#5767d2")
      (function-name "#aC2f28")
      (doc "#148c18")
      (sidebar-fg "#80329e")
      (sidebar-bg "#efe5f5")
      (line-number-bg "#e0e4f8")
      )

  (custom-theme-set-faces
   'oriole

   ;; Basic coloring
   `(default ((,class (:foreground ,fg :background ,bg))))
   `(cursor ((,class (:background ,cursor))))
   `(region ((,class (:background ,region))))
   `(minibuffer-prompt ((,class (:foreground ,keyword :bold t))))
   `(button ((,class (:foreground ,keyword :underline t :weight bold))))
   `(custom-visibility ((,class (:foreground ,keyword :underline t :weight bold))))
   `(link ((,class (:foreground ,keyword :underline t))))

   ;; matching
   `(show-paren-match ((,class (:foreground ,sidebar-fg :background ,line-number-bg
					    :weight bold))))
   `(show-paren-mismatch ((,class (:foreground "white" :background "red"
					      :weight bold))))
   
   ;;mode line
   `(line-number ((,class (:foreground ,comment :background ,line-number-bg :slant normal))))
   `(line-number-current-line ((,class (:foreground ,line-number-bg :background ,comment))))
   `(italic ((t (:slant italic))))
   `(mode-line-inactive ((,class (:foreground ,sidebar-fg :background ,sidebar-bg))))
   `(highlight-indentation-face ((,class (:background ,sidebar-bg))))
   `(mode-line-active ((,class (:foreground ,sidebar-active-fg :background ,sidebar-active-bg))))
  
   ;;Font-lock faces
   `(font-lock-doc-face ((,class (:foreground ,doc))))
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-preprocessor-face ((,class (:foreground ,keyword :slant normal))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword :slant normal))))
   `(font-lock-builtin-face ((,class (:foreground ,builtin :slant normal))))
   `(font-lock-number-face ((,class (:foreground ,number :slant normal))))
   `(font-lock-string-face ((,class (:foreground ,string))))
   `(font-lock-type-face ((,class (:foreground ,type :bold t :slant normal))))
   `(font-lock-operator-face ((,class (:foreground ,operator))))
   `(font-lock-variable-name-face ((,class (:foreground ,decleration :weight normal))))
   `(font-lock-function-name-face ((,class (:foreground ,function-name :bold t))))
   `(font-lock-function-call-face ((,class (:foreground ,function-name :bold t))))
   `(font-lock-constant-face ((,class (:foreground ,keyword))))
   `(font-lock-warning-face ((,class (:foreground "red" :weight bold))))
      
   ;; file systems
   `(neo-root-gdir-face ((,class (:foreground ,fg :underline t))))
   `(neo-file-link-face ((,class (:foreground ,fg))))
   `(neo-expand-btn-face ((,class (:foreground ,keyword :background ,bg
				     :slant normal :weight bold ))))
   `(neo-dir-link-face ((,class (:foreground ,keyword :weight bold))))

   `(dired-header ((,class (:foreground ,sidebar-fg :background ,sidebar-bg :weight bold))))
   `(dired-directory ((,class (:foreground ,keyword :weight bold))))

   ;;magit
   `(magit-section-heading ((,class (:foreground ,variable :background ,bg :weight bold))))
   `(magit-section-highlight ((,class (:background ,light-grey))))
   `(magit-branch-local ((,class (:foreground "blue"))))
   `(magit-branch-remote ((,class (:foreground ,string))))

   ;; `(magit-diff-

   `(magit-diff-file-heading ((,class (:foreground ,keyword :weight bold))))
   `(magit-diff-added ((,class (:foreground ,doc :background ,bg))))
   `(magit-diff-context ((,class (:foreground ,light-grey :background ,bg))))
   `(magit-diff-removed ((,class (:foreground ,function-name :background ,bg))))
   `(magit-diff-hunk-heading ((,class (:foreground "grey-20" :background "grey-90"))))
   `(magit-diff-hunk-heading-highlight ((,class (:foreground "grey-20" :background "grey-80"))))
   
   `(magit-diff-context-highlight ((,class (:foreground ,sidebar-fg :background ,sidebar-bg))))
   `(magit-diff-added-highlight ((,class (:foreground ,sidebar-active-fg :background ,sidebar-active-bg))))
   `(magit-diff-removed-highlight ((,class (:foreground ,function-name :background ,sidebar-active-bg))))
   
   ;; org mode
   `(org-table ((,class (:foreground ,operator))))
   `(completions-common-part ((,class (:foreground ,keyword))))
   `(menu ((,class (:foreground "#ffffff" :background "#3f1278"))))
   )
  )

(provide-theme 'oriole)
