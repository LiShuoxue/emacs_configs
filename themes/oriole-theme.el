;; oriole-theme.el
;; A customized, eye-caring light theme inspired by Quiet Light theme in vscode
;; and the apparel of An Lingrong (from TV series 'Legend of Zhen Huan').

;; Copyright (C) 2025-2026 Shuoxue Li

;; Version 0.1

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.


;; Code :
(deftheme oriole
  "A customized, eye-caring light theme inspired by Quiet Light theme in vscode
and the apparel of An Lingrong (from TV series 'Legend of Zhen Huan').")


(let ((class '((class color)))      
      (dark-grey "#555555")
      (light-grey "#bbbbbb")
      (fg "#68174e")
      (sidebar-fg "#80329e")
      (bg "#effaec")
      (sidebar-bg "#efe5f5")
      (line-number-bg "#e0e4f8")
      (sidebar-active-bg "#c5ebba")
      (cursor "#114514")
      (highlight "#e4e4e4")
      (indentation-face "#c5ebba")
      (sidebar-active-fg "#204c8b")
      (region "#f5f7d3")
      (comment "#888888")
      (keyword "#2665a9")
      (decleration "#ba2fa7")
      (number "#A76023")
      (builtin "#005f87")
      (string "#2f6a21")
      (type "#6138e8")
      (variable "#870000")
      (operator "#5767d2")
      (function-name "#aC2f28")
      (doc "#148c18")
      )

  (custom-theme-set-faces
   'oriole

   ;; Basic coloring
   `(default ((,class (:foreground ,fg :background ,bg))))
   `(cursor ((,class (:background ,cursor))))
   `(region ((,class (:background ,region))))
   `(minibuffer-prompt ((,class (:foreground ,keyword :background ,line-number-bg :bold t))))
   `(button ((,class (:foreground ,keyword :underline t :weight bold))))
   `(custom-visibility ((,class (:foreground ,keyword :underline t :weight bold))))
   `(link ((,class (:foreground ,keyword :underline t))))
   `(completions-common-part ((,class (:foreground ,keyword))))
   `(menu ((,class (:foreground "#ffffff" :background "#3f1278"))))

   ;; matching parentheses
   `(show-paren-match ((,class (:foreground ,sidebar-fg :background ,line-number-bg
					    :weight bold))))
   `(show-paren-mismatch ((,class (:foreground "white" :background "red"
					      :weight bold))))

   ;;mode line
   `(line-number ((,class (:foreground ,comment :background ,line-number-bg :slant normal))))
   `(line-number-current-line ((,class (:foreground ,line-number-bg :background ,comment))))
   `(italic ((t (:slant italic))))
   `(mode-line-inactive ((,class (:foreground ,sidebar-fg :background ,sidebar-bg))))
   `(highlight-indentation-face ((,class (:background "#dddddd"))))
   `(mode-line-active ((,class (:foreground ,sidebar-active-fg :background ,sidebar-active-bg))))
   `(mode-line-buffer-id ((,class (:foreground ,comment :background unspecified :bold t))))
   
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
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,builtin))))
   `(font-lock-warning-face ((,class (:foreground "red" :weight bold))))
      
   ;; file systems
   `(neo-root-gdir-face ((,class (:foreground ,fg :underline t))))
   `(neo-file-link-face ((,class (:foreground ,fg))))
   `(neo-expand-btn-face ((,class (:foreground ,keyword :background ,bg
				     :slant normal :weight bold ))))
   `(neo-dir-link-face ((,class (:foreground ,keyword :weight bold))))

   `(dired-header ((,class (:foreground ,sidebar-fg :background ,sidebar-bg :weight bold))))
   `(dired-directory ((,class (:foreground ,keyword :background ,bg :weight bold))))

   ;; magit
   `(magit-section-heading ((,class (:foreground ,variable :background ,bg :weight bold))))
   `(magit-section-highlight ((,class (:background ,sidebar-bg))))
   `(magit-branch-local ((,class (:foreground "blue"))))
   `(magit-branch-remote ((,class (:foreground ,string))))
   `(magit-hash ((,class (:foreground ,comment :background ,bg :slant italic))))

   ;; magit diff
   `(magit-diffstat-added ((,class (:foreground ,doc))))
   `(magit-diffstat-removed ((,class (:foreground ,function-name))))
   `(magit-diff-file-heading ((,class (:foreground ,keyword :weight bold))))
   `(magit-diff-added ((,class (:foreground ,doc :background ,bg))))
   `(magit-diff-file-heading-highlight ((,class (:foreground ,keyword :background ,line-number-bg))))							    
   `(magit-diff-context ((,class (:foreground ,light-grey :background ,bg))))
   `(magit-diff-removed ((,class (:foreground ,function-name :background ,bg))))
   `(magit-diff-hunk-heading ((,class (:foreground "grey-20" :background "grey-90"))))
   `(magit-diff-hunk-heading-highlight ((,class (:foreground "grey-20" :background "grey-80"))))
   `(magit-diff-context-highlight ((,class (:foreground ,sidebar-fg :background ,sidebar-bg))))
   `(magit-diff-added-highlight ((,class (:foreground ,sidebar-active-fg :background ,sidebar-active-bg))))
   `(magit-diff-removed-highlight ((,class (:foreground ,function-name :background ,sidebar-active-bg))))
   `(magit-diff-whitespace-warning ((,class (:foreground "white" :background "red"))))
   `(magit-diff-revision-summary ((,class (:foreground ,fg :background ,bg :weight bold :underline t))))
   `(magit-diff-revision-summary-highlight ((,class (:foreground ,bg :background ,fg :weight bold :underline t))))

   ;;flymake
   `(flymake-error ((,class (:foreground "red" :background "linen" :weight normal :underline t))))
   `(flymake-warning ((,class (:foreground "orange" :background "cornsilk" :weight normal :underline t))))
   
   ;; org mode
   `(org-table ((,class (:foreground ,operator))))
   `(org-document-info-keyword ((,class (:foreground ,comment :background ,bg :bold t :slant italic))))
   `(org-document-title ((,class (:foreground ,dark-grey :background ,bg :bold t :slant normal))))
   `(org-meta-line ((,class (:foreground ,comment :background ,bg))))
   
   ;;hl-todo
   `(hl-todo ((,class (:background "#ffffff" :weight normal))))

   )
  )


(defface my-todo-face
  '((t (:foreground "#148c18" :weight bold :slant italic)))
  "Face for highlighting keywords like TODO NOTE FIXME etc..")

(defun my-todo-keywords ()
  (font-lock-add-keywords
   nil
   '(("\\<\\(TODO\\|FIXME\\|NOTE\\|BUG\\|WARN\\):?" 1 'my-todo-face t)
     )
   )
  )
(add-hook 'prog-mode-hook #'my-todo-keywords)

(provide-theme 'oriole)
