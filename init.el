;;; init.el --- Main configuration -*- lexical-binding: t; -*-

;; `user-emacs-directory' is set by Emacs itself (and honours
;; --init-directory), so only `load-path' needs extending here.
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

;; use-package has been built into Emacs since 29.1.
(require 'use-package)
(setq use-package-always-ensure t)

;; modus-themes is not built in and `init-ui.el' requires it directly rather
;; than through use-package, so make sure it exists before the modules load.
(unless (package-installed-p 'modus-themes)
  (package-refresh-contents)
  (package-install 'modus-themes))

;; Custom settings.
(setq custom-file (locate-user-emacs-file "custom.el"))

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
