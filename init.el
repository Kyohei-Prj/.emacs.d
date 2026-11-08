;;; init.el --- Main configuration -*- lexical-binding: t; -*-

(setq user-emacs-directory
      (expand-file-name "~/.emacs.d/"))

(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

;; Package repositories.
(require 'package)

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("gnu"    . 10)
        ("nongnu" . 5)
        ("melpa"  . 0)))

(package-initialize)

;; use-package ships with Emacs 31.1.
(require 'use-package)
(setq use-package-always-ensure t)

(unless (package-installed-p 'modus-themes)
  (package-refresh-contents)
  (package-install 'modus-themes))

;; Custom settings.
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))

(load custom-file 'noerror 'nomessage)

;; Load modules.
(require 'init-core)
(require 'init-ui)
(require 'init-completion)
(require 'init-navigation)
(require 'init-treesit)
(require 'init-lsp)
(require 'init-languages)
(require 'init-git)
(require 'init-terminal)
(require 'init-editing)

;;; init.el ends here
