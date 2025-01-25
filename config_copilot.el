
;; Configuration of github copilot

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 4))
  (add-to-list 'copilot-indentation-alist '(org-mode 4))
  (add-to-list 'copilot-indentation-alist '(text-mode 4))
  (add-to-list 'copilot-indentation-alist '(closure-mode 4))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 4))
)
