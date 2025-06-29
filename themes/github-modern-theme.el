;;; github-modern-theme.el --- The GitHub Modern color theme.

;; Copyright (C) 2016-2017 Philip Arvidsson

;; Author: Philip Arvidsson <philip@philiparvidsson.com>
;; URL: https://github.com/philiparvidsson/GitHub-Modern-Theme-for-Emacs
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is based on the Zenburn theme file by Bozhidar Batsov.

;;; Credits:

;; Bozhidar Batsov created the Zenburn theme file which this file is based on.

;;; Code:

(deftheme github-modern "The GitHub Modern color theme")

;;; Color Palette

(defvar github-default-colors-alist
  '(("github-black"                  . "#000000")
    ("github-border"                 . "#d0d0d0")
    ("github-comment"                . "#6a737d")
    ("github-constant"               . "#005cc5")
    ("github-diff-added"             . "#e6ffed")
    ("github-diff-added-highlight"   . "#acf2bd")
    ("github-diff-changed"           . "#ffe1b9");; correct?
    ("github-diff-changed-highlight" . "#ffc86f");; correct?
    ("github-diff-removed"           . "#ffeef0")
    ("github-diff-removed-highlight" . "#fdb8c0")
    ("github-function"               . "#6f42c1")
    ("github-highlight"              . "#fffbdd")
    ("github-html-tag"               . "#22863a")
    ("github-keyword"                . "#d73a49")
    ("github-selection"              . "#3390ff")
    ("github-string"                 . "#032f62")
    ("github-text"                   . "#24292e")
    ("github-white"                  . "#ffffff"))
  "List of GitHub colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defvar github-override-colors-alist
  '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist before loading the theme.")

(defvar github-colors-alist
  (append github-default-colors-alist github-override-colors-alist))

(defmacro github-with-color-variables (&rest body)
  "`let' bind all colors defined in `github-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   github-colors-alist))
     ,@body))

;;; Theme Faces
(github-with-color-variables
  (custom-theme-set-faces
   'github-modern
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,github-keyword :underline t :weight bold))))
   `(link-visited ((t (:foreground ,github-text :underline t :weight normal))))
   `(default ((t (:foreground ,github-text :background ,github-white))))
   `(cursor ((t (:foreground ,github-text :background ,github-text))))
   `(escape-glyph ((t (:foreground ,github-keyword :bold t))))
   `(fringe ((t (:foreground ,github-text :background ,github-white))))
   `(header-line ((t (:foreground ,github-keyword
                                  :background ,github-selection
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,github-highlight))))
   `(success ((t (:foreground ,github-comment :weight bold))))
   `(warning ((t (:foreground ,github-text :weight bold))))
   `(tooltip ((t (:foreground ,github-text :background ,github-white))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,github-keyword))))
   `(compilation-enter-directory-face ((t (:foreground ,github-comment))))
   `(compilation-error-face ((t (:foreground ,github-text :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,github-text))))
   `(compilation-info-face ((t (:foreground ,github-text))))
   `(compilation-info ((t (:foreground ,github-constant :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,github-comment))))
   `(compilation-line-face ((t (:foreground ,github-keyword))))
   `(compilation-line-number ((t (:foreground ,github-keyword))))
   `(compilation-message-face ((t (:foreground ,github-text))))
   `(compilation-warning-face ((t (:foreground ,github-text :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,github-comment :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,github-string :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,github-keyword :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,github-text))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,github-text))))
   `(grep-error-face ((t (:foreground ,github-text :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,github-text))))
   `(grep-match-face ((t (:foreground ,github-text :weight bold))))
   `(match ((t (:background ,github-selection :foreground ,github-text :weight bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,github-white :weight bold :background ,github-selection))))
   `(isearch-fail ((t (:foreground ,github-border :background ,github-white))))
   `(lazy-highlight ((t (:foreground ,github-text :weight bold :background ,github-highlight))))

   `(menu ((t (:foreground ,github-text :background ,github-white))))
   `(minibuffer-prompt ((t (:foreground ,github-keyword))))
   `(mode-line
     ((,class (:foreground ,github-white
                           :background ,github-keyword))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,github-black :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,github-comment
                      :background ,github-white
                      :box (:line-width -1 :color ,github-border)))))
   `(region ((,class (:background ,github-selection))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,github-white))))
   `(trailing-whitespace ((t (:background ,github-string))))
   `(vertical-border ((t (:foreground ,github-border))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,github-keyword))))
   `(font-lock-comment-face ((t (:foreground ,github-comment))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,github-comment))))
   `(font-lock-constant-face ((t (:foreground ,github-constant))))
   `(font-lock-doc-face ((t (:foreground ,github-string))))
   `(font-lock-function-name-face ((t (:foreground ,github-function))))
   `(font-lock-keyword-face ((t (:foreground ,github-keyword))))
   `(font-lock-negation-char-face ((t (:foreground ,github-keyword))))
   `(font-lock-preprocessor-face ((t (:foreground ,github-keyword))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,github-keyword))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,github-comment))))
   `(font-lock-string-face ((t (:foreground ,github-string))))
   `(font-lock-type-face ((t (:foreground ,github-constant))))
   `(font-lock-variable-name-face ((t (:foreground ,github-text))))
   `(font-lock-warning-face ((t (:foreground ,github-text))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,github-text))))
   `(newsticker-default-face ((t (:foreground ,github-text))))
   `(newsticker-enclosure-face ((t (:foreground ,github-html-tag))))
   `(newsticker-extra-face ((t (:foreground ,github-white :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,github-text))))
   `(newsticker-immortal-item-face ((t (:foreground ,github-comment))))
   `(newsticker-new-item-face ((t (:foreground ,github-text))))
   `(newsticker-obsolete-item-face ((t (:foreground ,github-string))))
   `(newsticker-old-item-face ((t (:foreground ,github-white))))
   `(newsticker-statistics-face ((t (:foreground ,github-text))))
   `(newsticker-treeview-face ((t (:foreground ,github-text))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,github-comment))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,github-text))))
   `(newsticker-treeview-new-face ((t (:foreground ,github-text :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,github-string))))
   `(newsticker-treeview-old-face ((t (:foreground ,github-white))))
   `(newsticker-treeview-selection-face ((t (:background ,github-selection :foreground ,github-keyword))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,github-text :background ,github-white :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,github-comment :background ,github-white :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
     ((t (:foreground ,github-text :background ,github-white :inverse-video nil))))
   `(aw-leading-char-face ((t (:foreground ,github-white :background ,github-keyword :weight bold))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,github-text))))
   `(android-mode-error-face ((t (:foreground ,github-text :weight bold))))
   `(android-mode-info-face ((t (:foreground ,github-text))))
   `(android-mode-verbose-face ((t (:foreground ,github-comment))))
   `(android-mode-warning-face ((t (:foreground ,github-keyword))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,github-function :weight bold))))
   `(anzu-match-1 ((t (:foreground ,github-white :background ,github-comment))))
   `(anzu-match-2 ((t (:foreground ,github-white :background ,github-text))))
   `(anzu-match-3 ((t (:foreground ,github-white :background ,github-text))))
   `(anzu-replace-to ((t (:inherit anzu-replace-highlight :foreground ,github-keyword))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,github-string :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,github-keyword))))
   `(font-latex-italic-face ((t (:foreground ,github-function :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,github-text))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,github-keyword :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,github-string))))
   `(agda2-highlight-symbol-face ((t (:foreground ,github-text))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,github-constant))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,github-text))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,github-text))))
   `(agda2-highlight-datatype-face ((t (:foreground ,github-text))))
   `(agda2-highlight-function-face ((t (:foreground ,github-text))))
   `(agda2-highlight-module-face ((t (:foreground ,github-constant))))
   `(agda2-highlight-error-face ((t (:foreground ,github-white :background ,github-text))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,github-white :background ,github-text))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,github-white :background ,github-text))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,github-white :background ,github-text))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,github-white :background ,github-text))))
   `(agda2-highlight-typechecks-face ((t (:background ,github-text))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,github-white :foreground ,github-text))))
   `(ac-completion-face ((t (:background ,github-selection :foreground ,github-text))))
   `(ac-selection-face ((t (:background ,github-selection :foreground ,github-text))))
   `(popup-tip-face ((t (:background ,github-text :foreground ,github-white))))
   `(popup-scroll-bar-foreground-face ((t (:background ,github-text))))
   `(popup-scroll-bar-background-face ((t (:background ,github-comment))))
   `(popup-isearch-match ((t (:background ,github-white :foreground ,github-text))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground ,github-text :background ,github-white :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,github-html-tag :background ,github-white :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,github-keyword :background ,github-white :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,github-text :background ,github-white :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,github-function :background ,github-white :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,github-text :background ,github-white))))
   `(company-tooltip-annotation ((t (:foreground ,github-text :background ,github-white))))
   `(company-tooltip-annotation-selection ((t (:foreground ,github-text :background ,github-selection))))
   `(company-tooltip-selection ((t (:foreground ,github-text :background ,github-selection))))
   `(company-tooltip-mouse ((t (:background ,github-selection))))
   `(company-tooltip-common ((t (:foreground ,github-comment))))
   `(company-tooltip-common-selection ((t (:foreground ,github-comment))))
   `(company-scrollbar-fg ((t (:background ,github-text))))
   `(company-scrollbar-bg ((t (:background ,github-white))))
   `(company-preview ((t (:background ,github-comment))))
   `(company-preview-common ((t (:foreground ,github-comment :background ,github-selection))))
;;;;; bm
   `(bm-face ((t (:background ,github-text :foreground ,github-white))))
   `(bm-fringe-face ((t (:background ,github-text :foreground ,github-white))))
   `(bm-fringe-persistent-face ((t (:background ,github-comment :foreground ,github-white))))
   `(bm-persistent-face ((t (:background ,github-comment :foreground ,github-white))))
;;;;; cider
   `(cider-result-overlay-face ((t (:background unspecified))))
   `(cider-enlightened-face ((t (:box (:color ,github-text :line-width -1)))))
   `(cider-enlightened-local-face ((t (:weight bold :foreground ,github-text))))
   `(cider-deprecated-face ((t (:background ,github-text))))
   `(cider-instrumented-face ((t (:box (:color ,github-string :line-width -1)))))
   `(cider-traced-face ((t (:box (:color ,github-function :line-width -1)))))
   `(cider-test-failure-face ((t (:background ,github-text))))
   `(cider-test-error-face ((t (:background ,github-text))))
   `(cider-test-success-face ((t (:background ,github-comment))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,github-function))))
   `(circe-my-message-face ((t (:foreground ,github-text))))
   `(circe-fool-face ((t (:foreground ,github-text))))
   `(circe-topic-diff-removed-face ((t (:foreground ,github-string :weight bold))))
   `(circe-originator-face ((t (:foreground ,github-text))))
   `(circe-server-face ((t (:foreground ,github-comment))))
   `(circe-topic-diff-new-face ((t (:foreground ,github-text :weight bold))))
   `(circe-prompt-face ((t (:foreground ,github-text :background ,github-white :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,github-text)))
   `(context-coloring-level-1-face ((t :foreground ,github-function)))
   `(context-coloring-level-2-face ((t :foreground ,github-constant)))
   `(context-coloring-level-3-face ((t :foreground ,github-keyword)))
   `(context-coloring-level-4-face ((t :foreground ,github-text)))
   `(context-coloring-level-5-face ((t :foreground ,github-text)))
   `(context-coloring-level-6-face ((t :foreground ,github-keyword)))
   `(context-coloring-level-7-face ((t :foreground ,github-comment)))
   `(context-coloring-level-8-face ((t :foreground ,github-text)))
   `(context-coloring-level-9-face ((t :foreground ,github-text)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,github-text :foreground ,github-white))))
   `(ctbl:face-continue-bar ((t (:background ,github-highlight :foreground ,github-white))))
   `(ctbl:face-row-select ((t (:background ,github-function :foreground ,github-white))))
;;;;; diff
   `(diff-added          ((t (:background ,github-diff-added :foreground ,github-text))))
   `(diff-changed        ((t (:background ,github-diff-changed :foreground ,github-text))))
   `(diff-removed        ((t (:background ,github-diff-removed :foreground ,github-text))))
   `(diff-refine-added   ((t (:background ,github-diff-added-highlight :foreground ,github-text))))
   `(diff-refine-change  ((t (:background ,github-diff-changed-highlight :foreground ,github-text))))
   `(diff-refine-removed ((t (:background ,github-diff-removed-highlight :foreground ,github-text))))
   `(diff-header ((,class (:background ,github-white))
                  (t (:background ,github-text :foreground ,github-white))))
   `(diff-file-header
     ((,class (:background ,github-white :foreground ,github-text :bold t))
      (t (:background ,github-text :foreground ,github-white :bold t))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,github-text :background ,github-diff-added))))
   `(diff-hl-delete ((,class (:foreground ,github-text :background ,github-diff-removed-highlight))))
   `(diff-hl-insert ((,class (:foreground ,github-text :background ,github-diff-added-highlight))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,github-white)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,github-text))))
   `(diredp-compressed-file-suffix ((t (:foreground ,github-text))))
   `(diredp-date-time ((t (:foreground ,github-text))))
   `(diredp-deletion ((t (:foreground ,github-keyword))))
   `(diredp-deletion-file-name ((t (:foreground ,github-string))))
   `(diredp-dir-heading ((t (:foreground ,github-text :background ,github-selection))))
   `(diredp-dir-priv ((t (:foreground ,github-function))))
   `(diredp-exec-priv ((t (:foreground ,github-string))))
   `(diredp-executable-tag ((t (:foreground ,github-text))))
   `(diredp-file-name ((t (:foreground ,github-text))))
   `(diredp-file-suffix ((t (:foreground ,github-comment))))
   `(diredp-flag-mark ((t (:foreground ,github-keyword))))
   `(diredp-flag-mark-line ((t (:foreground ,github-text))))
   `(diredp-ignored-file-name ((t (:foreground ,github-string))))
   `(diredp-link-priv ((t (:foreground ,github-keyword))))
   `(diredp-mode-line-flagged ((t (:foreground ,github-keyword))))
   `(diredp-mode-line-marked ((t (:foreground ,github-text))))
   `(diredp-no-priv ((t (:foreground ,github-text))))
   `(diredp-number ((t (:foreground ,github-text))))
   `(diredp-other-priv ((t (:foreground ,github-text))))
   `(diredp-rare-priv ((t (:foreground ,github-text))))
   `(diredp-read-priv ((t (:foreground ,github-comment))))
   `(diredp-symlink ((t (:foreground ,github-keyword))))
   `(diredp-write-priv ((t (:foreground ,github-text))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,github-string :weight bold))))
   `(dired-async-message ((t (:foreground ,github-keyword :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,github-keyword))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,github-text :background ,github-diff-removed))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,github-text :background ,github-text))))
   `(ediff-current-diff-B ((t (:foreground ,github-text :background ,github-diff-added))))
   `(ediff-current-diff-C ((t (:foreground ,github-text :background ,github-text))))
   `(ediff-even-diff-A ((t (:background ,github-white))))
   `(ediff-even-diff-Ancestor ((t (:background ,github-white))))
   `(ediff-even-diff-B ((t (:background ,github-white))))
   `(ediff-even-diff-C ((t (:background ,github-white))))
   `(ediff-fine-diff-A ((t (:foreground ,github-text :background ,github-diff-removed-highlight :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,github-text :background ,github-text weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,github-text :background ,github-diff-added-highlight :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,github-text :background ,github-text :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,github-white))))
   `(ediff-odd-diff-Ancestor ((t (:background ,github-white))))
   `(ediff-odd-diff-B ((t (:background ,github-white))))
   `(ediff-odd-diff-C ((t (:background ,github-white))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,github-text))))
   `(egg-help-header-1 ((t (:foreground ,github-keyword))))
   `(egg-help-header-2 ((t (:foreground ,github-html-tag))))
   `(egg-branch ((t (:foreground ,github-keyword))))
   `(egg-branch-mono ((t (:foreground ,github-keyword))))
   `(egg-term ((t (:foreground ,github-keyword))))
   `(egg-diff-add ((t (:foreground ,github-constant))))
   `(egg-diff-del ((t (:foreground ,github-text))))
   `(egg-diff-file-header ((t (:foreground ,github-text))))
   `(egg-section-title ((t (:foreground ,github-keyword))))
   `(egg-stash-mono ((t (:foreground ,github-constant))))
;;;;; elfeed
   `(elfeed-log-error-level-face ((t (:foreground ,github-string))))
   `(elfeed-log-info-level-face ((t (:foreground ,github-text))))
   `(elfeed-log-warn-level-face ((t (:foreground ,github-keyword))))
   `(elfeed-search-date-face ((t (:foreground ,github-text :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,github-comment))))
   `(elfeed-search-feed-face ((t (:foreground ,github-function))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,github-keyword :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,github-text
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,github-text :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,github-keyword
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,github-comment :background ,github-white))))
   `(w3m-lnum-match ((t (:background ,github-selection
                                     :foreground ,github-text
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,github-keyword))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,github-text :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,github-text))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,github-keyword))))
   `(erc-keyword-face ((t (:foreground ,github-text :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,github-keyword :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,github-string :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,github-comment))))
   `(erc-pal-face ((t (:foreground ,github-text :weight bold))))
   `(erc-prompt-face ((t (:foreground ,github-text :background ,github-white :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,github-constant))))
   `(erc-underline-face ((t (:underline t))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,github-constant :background ,github-white))))
   `(ert-test-result-unexpected ((t (:foreground ,github-string :background ,github-white))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,github-keyword :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,github-text :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,github-keyword :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,github-text :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,github-text))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,github-keyword :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,github-function :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,github-comment :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-text) :inherit unspecified))
      (t (:foreground ,github-text :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-keyword) :inherit unspecified))
      (t (:foreground ,github-keyword :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-function) :inherit unspecified))
      (t (:foreground ,github-function :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,github-text :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,github-keyword :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,github-function :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-string)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,github-text :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-text)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,github-text :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-comment)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,github-comment :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-text) :inherit unspecified))
      (t (:foreground ,github-text :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-string) :inherit unspecified))
      (t (:foreground ,github-text :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,github-text))))
   `(ack-file ((t (:foreground ,github-text))))
   `(ack-line ((t (:foreground ,github-keyword))))
   `(ack-match ((t (:foreground ,github-text :background ,github-selection :weight bold))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,github-text :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,github-keyword  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,github-keyword  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,github-constant :weight bold))))
   `(git-gutter:deleted ((t (:foreground ,github-keyword :weight bold))))
   `(git-gutter:modified ((t (:foreground ,github-string :weight bold))))
   `(git-gutter:unchanged ((t (:foreground ,github-text :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,github-comment  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,github-string :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,github-text :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, github-text))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-to))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-server-opened ((t (:foreground ,github-comment :weight bold))))
   `(gnus-server-denied ((t (:foreground ,github-text :weight bold))))
   `(gnus-server-closed ((t (:foreground ,github-text :slant italic))))
   `(gnus-server-offline ((t (:foreground ,github-keyword :weight bold))))
   `(gnus-server-agent ((t (:foreground ,github-text :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,github-text))))
   `(gnus-summary-high-ancient ((t (:foreground ,github-text))))
   `(gnus-summary-high-read ((t (:foreground ,github-comment :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,github-text :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,github-text :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,github-text))))
   `(gnus-summary-low-read ((t (:foreground ,github-comment))))
   `(gnus-summary-low-ticked ((t (:foreground ,github-text :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,github-text))))
   `(gnus-summary-normal-ancient ((t (:foreground ,github-text))))
   `(gnus-summary-normal-read ((t (:foreground ,github-comment))))
   `(gnus-summary-normal-ticked ((t (:foreground ,github-text :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,github-text))))
   `(gnus-summary-selected ((t (:foreground ,github-keyword :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,github-text))))
   `(gnus-cite-10 ((t (:foreground ,github-text))))
   `(gnus-cite-11 ((t (:foreground ,github-keyword))))
   `(gnus-cite-2 ((t (:foreground ,github-constant))))
   `(gnus-cite-3 ((t (:foreground ,github-text))))
   `(gnus-cite-4 ((t (:foreground ,github-comment))))
   `(gnus-cite-5 ((t (:foreground ,github-text))))
   `(gnus-cite-6 ((t (:foreground ,github-comment))))
   `(gnus-cite-7 ((t (:foreground ,github-string))))
   `(gnus-cite-8 ((t (:foreground ,github-text))))
   `(gnus-cite-9 ((t (:foreground ,github-text))))
   `(gnus-group-news-1-empty ((t (:foreground ,github-keyword))))
   `(gnus-group-news-2-empty ((t (:foreground ,github-html-tag))))
   `(gnus-group-news-3-empty ((t (:foreground ,github-text))))
   `(gnus-group-news-4-empty ((t (:foreground ,github-text))))
   `(gnus-group-news-5-empty ((t (:foreground ,github-text))))
   `(gnus-group-news-6-empty ((t (:foreground ,github-white))))
   `(gnus-group-news-low-empty ((t (:foreground ,github-white))))
   `(gnus-signature ((t (:foreground ,github-keyword))))
   `(gnus-x ((t (:background ,github-text :foreground ,github-white))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,github-text))))
   `(guide-key/key-face ((t (:foreground ,github-comment))))
   `(guide-key/prefix-command-face ((t (:foreground ,github-text))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,github-comment
                      :background ,github-white
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,github-keyword
                      :background ,github-selection
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,github-highlight :underline nil))))
   `(helm-selection-line ((t (:background ,github-white))))
   `(helm-visible-mark ((t (:foreground ,github-white :background ,github-text))))
   `(helm-candidate-number ((t (:foreground ,github-constant :background ,github-selection))))
   `(helm-separator ((t (:foreground ,github-string :background ,github-white))))
   `(helm-time-zone-current ((t (:foreground ,github-comment :background ,github-white))))
   `(helm-time-zone-home ((t (:foreground ,github-string :background ,github-white))))
   `(helm-bookmark-addressbook ((t (:foreground ,github-text :background ,github-white))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,github-text :background ,github-white))))
   `(helm-bookmark-info ((t (:foreground ,github-comment :background ,github-white))))
   `(helm-bookmark-man ((t (:foreground ,github-keyword :background ,github-white))))
   `(helm-bookmark-w3m ((t (:foreground ,github-text :background ,github-white))))
   `(helm-buffer-not-saved ((t (:foreground ,github-string :background ,github-white))))
   `(helm-buffer-process ((t (:foreground ,github-function :background ,github-white))))
   `(helm-buffer-saved-out ((t (:foreground ,github-text :background ,github-white))))
   `(helm-buffer-size ((t (:foreground ,github-text :background ,github-white))))
   `(helm-ff-directory ((t (:foreground ,github-function :background ,github-white :weight bold))))
   `(helm-ff-file ((t (:foreground ,github-text :background ,github-white :weight normal))))
   `(helm-ff-executable ((t (:foreground ,github-comment :background ,github-white :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,github-string :background ,github-white :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,github-keyword :background ,github-white :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,github-white :background ,github-keyword :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,github-function :background ,github-white))))
   `(helm-grep-file ((t (:foreground ,github-text :background ,github-white))))
   `(helm-grep-finish ((t (:foreground ,github-comment :background ,github-white))))
   `(helm-grep-lineno ((t (:foreground ,github-text :background ,github-white))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,github-string :background ,github-white))))
   `(helm-match ((t (:foreground ,github-text :background ,github-selection :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,github-function :background ,github-white))))
   `(helm-mu-contacts-address-face ((t (:foreground ,github-text :background ,github-white))))
   `(helm-mu-contacts-name-face ((t (:foreground ,github-text :background ,github-white))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,github-text :background ,github-white))))
   `(helm-swoop-target-word-face ((t (:foreground ,github-keyword :background ,github-white :weight bold))))
;;;;; highlight-numbers
   `(highlight-numbers-number ((t (:foreground ,github-constant))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,github-highlight))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,github-highlight)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,github-white))
                   (t :weight bold)))
;;;;; hlinum
   `(linum-highlight-face ((t (:foreground ,github-comment :background ,github-highlight))))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,github-text :background ,github-white))))
   `(hydra-face-amaranth ((t (:foreground ,github-text :background ,github-white))))
   `(hydra-face-blue ((t (:foreground ,github-text :background ,github-white))))
   `(hydra-face-pink ((t (:foreground ,github-text :background ,github-white))))
   `(hydra-face-teal ((t (:foreground ,github-function :background ,github-white))))
;;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,github-comment :background ,github-white))))
   `(ivy-match-required-face ((t (:foreground ,github-string :background ,github-white))))
   `(ivy-remote ((t (:foreground ,github-text :background ,github-white))))
   `(ivy-subdir ((t (:foreground ,github-keyword :background ,github-white))))
   `(ivy-current-match ((t (:foreground ,github-keyword :weight bold :underline t))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,github-white))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,github-comment))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,github-comment))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,github-text))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,github-keyword :weight bold))))
   `(ido-only-match ((t (:foreground ,github-text :weight bold))))
   `(ido-subdir ((t (:foreground ,github-keyword))))
   `(ido-indicator ((t (:foreground ,github-keyword :background ,github-text))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,github-white :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,github-comment))))
   `(jabber-roster-user-online ((t (:foreground ,github-constant))))
   `(jabber-roster-user-dnd ((t (:foreground ,github-text))))
   `(jabber-roster-user-xa ((t (:foreground ,github-text))))
   `(jabber-roster-user-chatty ((t (:foreground ,github-text))))
   `(jabber-roster-user-error ((t (:foreground ,github-text))))
   `(jabber-rare-time-face ((t (:foreground ,github-text))))
   `(jabber-chat-prompt-local ((t (:foreground ,github-constant))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,github-text))))
   `(jabber-chat-prompt-system ((t (:foreground ,github-html-tag))))
   `(jabber-activity-face((t (:foreground ,github-text))))
   `(jabber-activity-personal-face ((t (:foreground ,github-keyword))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,github-text))))
   `(js2-error ((t (:foreground ,github-string :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,github-comment))))
   `(js2-jsdoc-type ((t (:foreground ,github-comment))))
   `(js2-jsdoc-value ((t (:foreground ,github-html-tag))))
   `(js2-function-param ((t (:foreground, github-text))))
   `(js2-external-variable ((t (:foreground ,github-text))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,github-comment))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,github-text))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,github-text))))
   `(js2-object-property ((t (:foreground ,github-keyword))))
   `(js2-magic-paren ((t (:foreground ,github-text))))
   `(js2-private-function-call ((t (:foreground ,github-function))))
   `(js2-function-call ((t (:foreground ,github-function))))
   `(js2-private-member ((t (:foreground ,github-constant))))
   `(js2-keywords ((t (:foreground ,github-text))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,github-text :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,github-text :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,github-white))))
   `(ledger-font-pending-face ((t (:foreground ,github-text weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,github-text))))
   `(ledger-font-posting-account-face ((t (:foreground ,github-constant))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,github-text))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,github-text))))
   `(ledger-font-posting-amount-face ((t (:foreground ,github-text))))
   `(ledger-occur-narrowed-face ((t (:foreground ,github-text :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,github-white))))
   `(ledger-font-comment-face ((t (:foreground ,github-comment))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,github-text :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,github-text :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,github-text :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,github-text :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,github-comment :background ,github-white))))
;;;;; lispy
   `(lispy-command-name-face ((t (:background ,github-highlight :inherit font-lock-function-name-face))))
   `(lispy-cursor-face ((t (:foreground ,github-white :background ,github-text))))
   `(lispy-face-hint ((t (:inherit highlight :foreground ,github-keyword))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,github-text))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,github-keyword))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,github-keyword :box t))))
   `(ruler-mode-default ((t (:foreground ,github-comment :background ,github-white))))

;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,github-constant))))
   `(lui-hilight-face ((t (:foreground ,github-comment :background ,github-white))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,github-comment :background ,github-selection))))
   `(macrostep-gensym-2
     ((t (:foreground ,github-text :background ,github-selection))))
   `(macrostep-gensym-3
     ((t (:foreground ,github-keyword :background ,github-selection))))
   `(macrostep-gensym-4
     ((t (:foreground ,github-text :background ,github-selection))))
   `(macrostep-gensym-5
     ((t (:foreground ,github-keyword :background ,github-selection))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   `(magit-section-highlight           ((t (:background ,github-white))))
   `(magit-section-heading             ((t (:foreground ,github-keyword :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,github-text :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,github-white  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,github-white
                                                        :foreground ,github-text :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,github-white))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,github-white))))
   `(magit-diff-hunk-heading-selection ((t (:background ,github-white
                                                        :foreground ,github-text))))
   `(magit-diff-lines-heading          ((t (:background ,github-text
                                                        :foreground ,github-white))))
   `(magit-diff-context-highlight      ((t (:background ,github-white
                                                        :foreground "grey70"))))
   `(magit-diffstat-added   ((t (:foreground ,github-constant))))
   `(magit-diffstat-removed ((t (:foreground ,github-string))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,github-keyword  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,github-comment :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,github-comment   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,github-text    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,github-text  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,github-comment  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,github-string    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,github-text))))
   `(magit-log-date      ((t (:foreground ,github-text))))
   `(magit-log-graph     ((t (:foreground ,github-text))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,github-text))))
   `(magit-sequence-stop ((t (:foreground ,github-comment))))
   `(magit-sequence-part ((t (:foreground ,github-keyword))))
   `(magit-sequence-head ((t (:foreground ,github-text))))
   `(magit-sequence-drop ((t (:foreground ,github-string))))
   `(magit-sequence-done ((t (:foreground ,github-text))))
   `(magit-sequence-onto ((t (:foreground ,github-text))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,github-comment))))
   `(magit-bisect-skip ((t (:foreground ,github-keyword))))
   `(magit-bisect-bad  ((t (:foreground ,github-string))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,github-selection :foreground ,github-text))))
   `(magit-blame-hash    ((t (:background ,github-selection :foreground ,github-text))))
   `(magit-blame-name    ((t (:background ,github-selection :foreground ,github-text))))
   `(magit-blame-date    ((t (:background ,github-selection :foreground ,github-text))))
   `(magit-blame-summary ((t (:background ,github-selection :foreground ,github-text
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,github-text))))
   `(magit-hash           ((t (:foreground ,github-text))))
   `(magit-tag            ((t (:foreground ,github-text :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,github-comment  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,github-text   :weight bold))))
   `(magit-branch-current ((t (:foreground ,github-text   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,github-text   :weight bold))))
   `(magit-refname        ((t (:background ,github-white :foreground ,github-text :weight bold))))
   `(magit-refname-stash  ((t (:background ,github-white :foreground ,github-text :weight bold))))
   `(magit-refname-wip    ((t (:background ,github-white :foreground ,github-text :weight bold))))
   `(magit-signature-good      ((t (:foreground ,github-comment))))
   `(magit-signature-bad       ((t (:foreground ,github-string))))
   `(magit-signature-untrusted ((t (:foreground ,github-keyword))))
   `(magit-cherry-unmatched    ((t (:foreground ,github-function))))
   `(magit-cherry-equivalent   ((t (:foreground ,github-text))))
   `(magit-reflog-commit       ((t (:foreground ,github-comment))))
   `(magit-reflog-amend        ((t (:foreground ,github-text))))
   `(magit-reflog-merge        ((t (:foreground ,github-comment))))
   `(magit-reflog-checkout     ((t (:foreground ,github-text))))
   `(magit-reflog-reset        ((t (:foreground ,github-string))))
   `(magit-reflog-rebase       ((t (:foreground ,github-text))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,github-comment))))
   `(magit-reflog-remote       ((t (:foreground ,github-function))))
   `(magit-reflog-other        ((t (:foreground ,github-function))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,github-text))))
   `(message-header-other ((t (:foreground ,github-comment))))
   `(message-header-to ((t (:foreground ,github-keyword :weight bold))))
   `(message-header-cc ((t (:foreground ,github-keyword :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,github-keyword :weight bold))))
   `(message-header-subject ((t (:foreground ,github-text :weight bold))))
   `(message-header-xheader ((t (:foreground ,github-comment))))
   `(message-mml ((t (:foreground ,github-keyword :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,github-text))))
   `(mew-face-header-from ((t (:foreground ,github-keyword))))
   `(mew-face-header-date ((t (:foreground ,github-comment))))
   `(mew-face-header-to ((t (:foreground ,github-string))))
   `(mew-face-header-key ((t (:foreground ,github-comment))))
   `(mew-face-header-private ((t (:foreground ,github-comment))))
   `(mew-face-header-important ((t (:foreground ,github-text))))
   `(mew-face-header-marginal ((t (:foreground ,github-text :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,github-string))))
   `(mew-face-header-xmew ((t (:foreground ,github-comment))))
   `(mew-face-header-xmew-bad ((t (:foreground ,github-string))))
   `(mew-face-body-url ((t (:foreground ,github-text))))
   `(mew-face-body-comment ((t (:foreground ,github-text :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,github-comment))))
   `(mew-face-body-cite2 ((t (:foreground ,github-text))))
   `(mew-face-body-cite3 ((t (:foreground ,github-text))))
   `(mew-face-body-cite4 ((t (:foreground ,github-keyword))))
   `(mew-face-body-cite5 ((t (:foreground ,github-string))))
   `(mew-face-mark-review ((t (:foreground ,github-text))))
   `(mew-face-mark-escape ((t (:foreground ,github-comment))))
   `(mew-face-mark-delete ((t (:foreground ,github-string))))
   `(mew-face-mark-unlink ((t (:foreground ,github-keyword))))
   `(mew-face-mark-refile ((t (:foreground ,github-comment))))
   `(mew-face-mark-unread ((t (:foreground ,github-text))))
   `(mew-face-eof-message ((t (:foreground ,github-comment))))
   `(mew-face-eof-part ((t (:foreground ,github-keyword))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,github-function :background ,github-white :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,github-white :background ,github-text :weight bold))))
   `(paren-face-no-match ((t (:foreground ,github-white :background ,github-string :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,github-text))))
   `(mingus-pausing-face ((t (:foreground ,github-text))))
   `(mingus-playing-face ((t (:foreground ,github-function))))
   `(mingus-playlist-face ((t (:foreground ,github-function ))))
   `(mingus-song-file-face ((t (:foreground ,github-keyword))))
   `(mingus-stopped-face ((t (:foreground ,github-string))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,github-keyword))))
   `(nav-face-button-num ((t (:foreground ,github-function))))
   `(nav-face-dir ((t (:foreground ,github-comment))))
   `(nav-face-hdir ((t (:foreground ,github-string))))
   `(nav-face-file ((t (:foreground ,github-text))))
   `(nav-face-hfile ((t (:foreground ,github-text))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,github-text    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,github-comment :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,github-text  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,github-comment   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,github-text  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,github-comment :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,github-text    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,github-text))))
   `(mu4e-trashed-face ((t (:foreground ,github-text :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,github-selection))))
   `(mumamo-background-chunk-submode2 ((t (:background ,github-white))))
   `(mumamo-background-chunk-submode3 ((t (:background ,github-white))))
   `(mumamo-background-chunk-submode4 ((t (:background ,github-white))))
;;;;; neotree
   `(neo-banner-face ((t (:foreground ,github-keyword :weight bold))))
   `(neo-header-face ((t (:foreground ,github-text))))
   `(neo-root-dir-face ((t (:foreground ,github-keyword :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,github-text))))
   `(neo-file-link-face ((t (:foreground ,github-text))))
   `(neo-expand-btn-face ((t (:foreground ,github-text))))
   `(neo-vc-default-face ((t (:foreground ,github-text))))
   `(neo-vc-user-face ((t (:foreground ,github-string :slant italic))))
   `(neo-vc-up-to-date-face ((t (:foreground ,github-text))))
   `(neo-vc-edited-face ((t (:foreground ,github-text))))
   `(neo-vc-needs-merge-face ((t (:foreground ,github-text))))
   `(neo-vc-unlocked-changes-face ((t (:foreground ,github-string :background ,github-text))))
   `(neo-vc-added-face ((t (:foreground ,github-text))))
   `(neo-vc-conflict-face ((t (:foreground ,github-text))))
   `(neo-vc-missing-face ((t (:foreground ,github-text))))
   `(neo-vc-ignored-face ((t (:foreground ,github-text))))
;;;;; org-mode
   `(org-agenda-clocking
     ((t (:bold t :background ,github-highlight))) t)
   `(org-agenda-date-today
     ((t (:foreground ,github-text :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,github-text :weight bold))))
   `(org-checkbox ((t (:background ,github-white :foreground ,github-text
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,github-text :underline t))))
   `(org-deadline-announce ((t (:foreground ,github-text))))
   `(org-done ((t (:bold t :weight bold :foreground ,github-html-tag))))
   `(org-formula ((t (:foreground ,github-text))))
   `(org-headline-done ((t (:foreground ,github-html-tag))))
   `(org-hide ((t (:foreground ,github-selection))))
   `(org-level-1 ((t (:foreground ,github-text))))
   `(org-level-2 ((t (:foreground ,github-constant))))
   `(org-level-3 ((t (:foreground ,github-constant))))
   `(org-level-4 ((t (:foreground ,github-text))))
   `(org-level-5 ((t (:foreground ,github-function))))
   `(org-level-6 ((t (:foreground ,github-comment))))
   `(org-level-7 ((t (:foreground ,github-text))))
   `(org-level-8 ((t (:foreground ,github-text))))
   `(org-link ((t (:foreground ,github-text :underline t))))
   `(org-scheduled ((t (:foreground ,github-constant))))
   `(org-scheduled-previously ((t (:foreground ,github-string))))
   `(org-scheduled-today ((t (:foreground ,github-keyword))))
   `(org-sexp-date ((t (:foreground ,github-keyword :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,github-comment))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,github-text))))
   `(org-todo ((t (:bold t :foreground ,github-string :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,github-string :weight bold :underline nil))))
   `(org-column ((t (:background ,github-selection))))
   `(org-column-title ((t (:background ,github-selection :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,github-text :background ,github-selection))))
   `(org-mode-line-clock-overrun ((t (:foreground ,github-white :background ,github-text))))
   `(org-ellipsis ((t (:foreground ,github-text :underline t))))
   `(org-footnote ((t (:foreground ,github-function :underline t))))
   `(org-document-title ((t (:foreground ,github-text))))
   `(org-document-info ((t (:foreground ,github-text))))
   `(org-habit-ready-face ((t :background ,github-comment)))
   `(org-habit-alert-face ((t :background ,github-text :foreground ,github-white)))
   `(org-habit-clear-face ((t :background ,github-text)))
   `(org-habit-overdue-face ((t :background ,github-text)))
   `(org-habit-clear-future-face ((t :background ,github-text)))
   `(org-habit-ready-future-face ((t :background ,github-comment)))
   `(org-habit-alert-future-face ((t :background ,github-text :foreground ,github-white)))
   `(org-habit-overdue-future-face ((t :background ,github-text)))
;;;;; outline
   `(outline-1 ((t (:foreground ,github-text))))
   `(outline-2 ((t (:foreground ,github-constant))))
   `(outline-3 ((t (:foreground ,github-constant))))
   `(outline-4 ((t (:foreground ,github-text))))
   `(outline-5 ((t (:foreground ,github-function))))
   `(outline-6 ((t (:foreground ,github-comment))))
   `(outline-7 ((t (:foreground ,github-text))))
   `(outline-8 ((t (:foreground ,github-text))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,github-text :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,github-string :inherit mode-line))))
   `(powerline-active2 ((t (:background ,github-keyword :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,github-white :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,github-white :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,github-text :background ,github-white))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,github-white :background ,github-text))))
   `(proof-error-face ((t (:foreground ,github-text :background ,github-text))))
   `(proof-highlight-dependency-face ((t (:foreground ,github-white :background ,github-comment))))
   `(proof-highlight-dependent-face ((t (:foreground ,github-white :background ,github-comment))))
   `(proof-locked-face ((t (:background ,github-comment))))
   `(proof-mouse-highlight-face ((t (:foreground ,github-white :background ,github-comment))))
   `(proof-queue-face ((t (:background ,github-comment))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,github-comment))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,github-white))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,github-white))))
   `(proof-warning-face ((t (:foreground ,github-white :background ,github-comment))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,github-comment))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,github-constant))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,github-comment))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,github-function))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,github-comment))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,github-keyword))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,github-comment))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,github-comment))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,github-comment))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,github-comment))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,github-comment))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,github-comment))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,github-comment))))
   `(rcirc-other-nick ((t (:foreground ,github-comment))))
   `(rcirc-bright-nick ((t (:foreground ,github-keyword))))
   `(rcirc-dim-nick ((t (:foreground ,github-comment))))
   `(rcirc-server ((t (:foreground ,github-comment))))
   `(rcirc-server-prefix ((t (:foreground ,github-comment))))
   `(rcirc-timestamp ((t (:foreground ,github-comment))))
   `(rcirc-nick-in-message ((t (:foreground ,github-keyword))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,github-keyword :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,github-keyword :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,github-comment))))
   `(rpm-spec-doc-face ((t (:foreground ,github-comment))))
   `(rpm-spec-ghost-face ((t (:foreground ,github-string))))
   `(rpm-spec-macro-face ((t (:foreground ,github-keyword))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,github-string))))
   `(rpm-spec-package-face ((t (:foreground ,github-string))))
   `(rpm-spec-section-face ((t (:foreground ,github-keyword))))
   `(rpm-spec-tag-face ((t (:foreground ,github-comment))))
   `(rpm-spec-var-face ((t (:foreground ,github-string))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,github-comment))))
   `(rst-level-2-face ((t (:foreground ,github-comment))))
   `(rst-level-3-face ((t (:foreground ,github-constant))))
   `(rst-level-4-face ((t (:foreground ,github-comment))))
   `(rst-level-5-face ((t (:foreground ,github-function))))
   `(rst-level-6-face ((t (:foreground ,github-comment))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,github-keyword :bold t))))
   `(sh-quoted-exec ((t (:foreground ,github-string))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,github-comment :background ,github-white :weight bold))))
   `(show-paren-match ((t (:foreground ,github-white :background ,github-keyword :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable GitHub for sml
   `(sml/global ((,class (:foreground ,github-comment :weight bold))))
   `(sml/modes ((,class (:foreground ,github-keyword :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,github-comment :weight bold))))
   `(sml/filename ((,class (:foreground ,github-keyword :weight bold))))
   `(sml/line-number ((,class (:foreground ,github-comment :weight bold))))
   `(sml/col-number ((,class (:foreground ,github-keyword :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,github-constant :weight bold))))
   `(sml/prefix ((,class (:foreground ,github-comment))))
   `(sml/git ((,class (:foreground ,github-html-tag))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,github-comment :weight bold))))
   `(sml/read-only ((,class (:foreground ,github-comment))))
   `(sml/outside-modified ((,class (:foreground ,github-comment))))
   `(sml/modified ((,class (:foreground ,github-string))))
   `(sml/vc-edited ((,class (:foreground ,github-comment))))
   `(sml/charging ((,class (:foreground ,github-constant))))
   `(sml/discharging ((,class (:foreground ,github-comment))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,github-comment :background ,github-white :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,github-white :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,github-string))))
   `(slime-repl-inputed-output-face ((t (:foreground ,github-comment))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-string)))
      (t
       (:underline ,github-string))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-comment)))
      (t
       (:underline ,github-comment))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-keyword)))
      (t
       (:underline ,github-keyword))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-comment)))
      (t
       (:underline ,github-comment))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,github-comment))))
   `(speedbar-directory-face ((t (:foreground ,github-function))))
   `(speedbar-file-face ((t (:foreground ,github-comment))))
   `(speedbar-highlight-face ((t (:foreground ,github-white :background ,github-comment))))
   `(speedbar-selected-face ((t (:foreground ,github-string))))
   `(speedbar-separator-face ((t (:foreground ,github-white :background ,github-constant))))
   `(speedbar-tag-face ((t (:foreground ,github-keyword))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,github-comment
                                    :background ,github-white))))
   `(tabbar-selected ((t (:foreground ,github-comment
                                      :background ,github-white
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,github-comment
                                        :background ,github-white
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,github-white
                                       :background ,github-selection))))
   `(term-color-red ((t (:foreground ,github-comment
                                     :background ,github-comment))))
   `(term-color-green ((t (:foreground ,github-comment
                                       :background ,github-comment))))
   `(term-color-yellow ((t (:foreground ,github-comment
                                        :background ,github-keyword))))
   `(term-color-blue ((t (:foreground ,github-constant
                                      :background ,github-comment))))
   `(term-color-magenta ((t (:foreground ,github-comment
                                         :background ,github-string))))
   `(term-color-cyan ((t (:foreground ,github-function
                                      :background ,github-comment))))
   `(term-color-white ((t (:foreground ,github-comment
                                       :background ,github-comment))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,github-comment :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,github-comment :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,github-comment))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,github-keyword))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,github-function))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,github-highlight))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,github-comment ))))
   `(web-mode-css-prop-face ((t (:foreground ,github-constant))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,github-html-tag :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,github-html-tag))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,github-comment))))
   `(web-mode-html-attr-name-face ((t (:foreground ,github-function))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,github-html-tag))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,github-white))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,github-string))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,github-white :foreground ,github-white))))
   `(whitespace-hspace ((t (:background ,github-white :foreground ,github-white))))
   `(whitespace-tab ((t (:background ,github-comment))))
   `(whitespace-newline ((t (:foreground ,github-white))))
   `(whitespace-trailing ((t (:background ,github-string))))
   `(whitespace-line ((t (:background ,github-white :foreground ,github-comment))))
   `(whitespace-space-before-tab ((t (:background ,github-comment :foreground ,github-comment))))
   `(whitespace-indentation ((t (:background ,github-keyword :foreground ,github-string))))
   `(whitespace-empty ((t (:background ,github-keyword))))
   `(whitespace-space-after-tab ((t (:background ,github-keyword :foreground ,github-string))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,github-comment))))
   `(wl-highlight-folder-many-face ((t (:foreground ,github-comment))))
   `(wl-highlight-folder-path-face ((t (:foreground ,github-comment))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,github-comment))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,github-comment))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,github-comment))))
   `(wl-highlight-message-citation-header ((t (:foreground ,github-comment))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,github-string))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,github-comment))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,github-comment))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,github-keyword))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,github-comment))))
   `(wl-highlight-message-headers-face ((t (:foreground ,github-comment))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,github-comment))))
   `(wl-highlight-message-header-contents ((t (:foreground ,github-comment))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,github-comment))))
   `(wl-highlight-message-signature ((t (:foreground ,github-comment))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,github-comment))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,github-comment))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,github-comment
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,github-comment))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,github-comment))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,github-keyword))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,github-comment))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,github-comment))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,github-constant))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,github-keyword :weight bold))))
   `(cscope-function-face ((t (:foreground ,github-function :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,github-string :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,github-white :background ,github-keyword))))
   `(cscope-separator-face ((t (:foreground ,github-string :weight bold
                                            :underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,github-selection))))
   `(yascroll:thumb-fringe ((t (:background ,github-selection :foreground ,github-selection))))

;;;;; elscreen
  `(elscreen-tab-background-face ((t (:background ,github-keyword))))
  `(elscreen-tab-control-face ((t (:foreground ,github-white :background ,github-keyword))))
  `(elscreen-tab-current-screen-face ((t (:foreground ,github-black :background ,github-selection))))
  `(elscreen-tab-other-screen-face ((t (:foreground ,github-text :background ,github-highlight))))
  ))

;;; Theme Variables
(github-with-color-variables
  (custom-theme-set-variables
   'github-modern
;;;;; ansi-color
   `(ansi-color-names-vector [,github-white ,github-string ,github-comment ,github-keyword
                                          ,github-comment ,github-comment ,github-function ,github-comment])
;;;;; fill-column-indicator
   `(fci-rule-color ,github-comment)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,github-string ,github-comment ,github-keyword ,github-comment ,github-constant
                    ,github-function ,github-keyword ,github-comment))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,github-comment . ,github-highlight))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,github-comment)
       ( 40. . ,github-string)
       ( 60. . ,github-comment)
       ( 80. . ,github-comment)
       (100. . ,github-comment)
       (120. . ,github-keyword)
       (140. . ,github-comment)
       (160. . ,github-comment)
       (180. . ,github-comment)
       (200. . ,github-comment)
       (220. . ,github-html-tag)
       (240. . ,github-constant)
       (260. . ,github-function)
       (280. . ,github-comment)
       (300. . ,github-constant)
       (320. . ,github-comment)
       (340. . ,github-keyword)
       (360. . ,github-comment)))
   `(vc-annotate-very-old-color ,github-comment)
   `(vc-annotate-background ,github-selection)
   ))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                  (file-name-directory load-file-name))))

(provide-theme 'github-modern)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; github-modern-theme.el ends here
