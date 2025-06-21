;; INSTALL PACKAGES
;; --------------------------------------

(condition-case nil
    (progn
      (require 'package)
      (setq package-archives '(
			       ("elpy" . "http://jorgenschaefer.github.io/packages/")
			       ("melpa" . "https://melpa.org/packages/")
                               ("gnu" . "https://elpa.gnu.org/packages/")
			       ("elpy" . "http://jorgenschaefer.github.io/packages/")
			   )
	    ))
      (package-initialize))
  (message "Failed to initialize all the package systems.")

(add-to-list 'package-archives
         '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	 '("gnu" . "https://elpa.gnu.org/packages/"))

;; activate all packages
(package-initialize)

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))


;; define list of packages to install
(defvar myPackages
  '(better-defaults
    elpy
    ein
    pyenv-mode
    slurm-mode
    magit
    activities
    ;; neotree
    vterm
    json-mode
    toml-mode
    ;nerd-icons
))

;; install all packages in list
(mapc #'(lambda (package)
      (unless (package-installed-p package)
      (package-install package)))
      myPackages)

(elpy-enable)

;; use straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

