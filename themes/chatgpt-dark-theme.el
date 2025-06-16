(deftheme chatgpt-dark
  "A theme inspired by ChatGPT's dark mode.")

(let ((class '((class color) (min-colors 89)))
      (fg "#E8E8E8")  ;; Light gray for foreground
      (bg "#000000")  ;; Dark blue-gray background
      (cursor "#FFFFFF") ;; White cursor
      (highlight "#333C57") ;; Highlight background
      (comment "#6A798F") ;; Gray-blue for comments
      (keyword "#5EA0EE") ;; Light blue for keywords
      (builtin "#FF9E64") ;; Orange for built-in functions
      (string "#A9DC76") ;; Green for strings
      (constant "#D67AD2") ;; Pinkish purple for constants
      (function-name "#FAC863") ;; Yellow for function names
      (variable "#F07178") ;; Light red for variables
      (region "#2C3247") ;; Slightly lighter region highlight
      (error "#F7768E") ;; Reddish pink for errors
      (warning "#E5C07B") ;; Yellow for warnings
      (info "#56B6C2")) ;; Teal for informational text

  (custom-theme-set-faces
   'chatgpt-dark
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,cursor))))
   `(region ((,class (:background ,region))))
   `(highlight ((,class (:background ,highlight))))
   `(fringe ((,class (:background ,bg))))
   `(mode-line ((,class (:background ,highlight :foreground ,fg))))
   `(mode-line-inactive ((,class (:background ,bg :foreground ,comment))))
   `(minibuffer-prompt ((,class (:foreground ,keyword))))
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-constant-face ((,class (:foreground ,constant))))
   `(font-lock-function-name-face ((,class (:foreground ,function-name))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword))))
   `(font-lock-string-face ((,class (:foreground ,string))))
   `(font-lock-type-face ((,class (:foreground ,keyword))))
   `(font-lock-variable-name-face ((,class (:foreground ,variable))))
   `(font-lock-warning-face ((,class (:foreground ,warning))))
   `(error ((,class (:foreground ,error :weight bold))))
   `(warning ((,class (:foreground ,warning :weight bold))))
   `(success ((,class (:foreground ,info :weight bold))))))

(provide-theme 'chatgpt-dark)
