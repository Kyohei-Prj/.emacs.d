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

;; Custom settings.  Loaded before anything is installed so that
;; `package-selected-packages' holds the curated list that later
;; `package-install' calls append to and save back.
(setq custom-file (locate-user-emacs-file "custom.el"))

(load custom-file 'noerror 'nomessage)

;; First-run bootstrap.  An empty `package-archive-contents' means this machine
;; has no archive cache to install from; `use-package :ensure' installs on demand
;; but never refreshes it.
(when (null package-archive-contents)
  (package-refresh-contents))

;; modus-themes is not built in and `init-ui.el' requires it directly rather
;; than through use-package, so make sure it exists before the modules load.
(unless (package-installed-p 'modus-themes)
  (package-install 'modus-themes))

;; `vterm.el' builds its Emacs module (vterm-module.so) from a top-level form
;; that runs on *load*, not on `M-x vterm': with no module present it prompts to
;; compile and errors if the answer is no.  That is a trap for `multi-vterm',
;; because package.el byte-compiles `multi-vterm.el' while installing it and
;; compiling that file loads `vterm.el' -- so on a fresh machine the compile of
;; `multi-vterm' dies part-way through, and a compile interrupted while it writes
;; its output leaves a stub multi-vterm.elc that shadows the real source from
;; then on (a .elc is preferred over its .el, so `M-x multi-vterm' fails with
;; "failed to define function multi-vterm").  Build the module first, while
;; nothing depends on it, so every later load of vterm.el just finds it.

;; Declared here so the `setq' in the bootstrap below is an ordinary dynamic
;; assignment; `vterm.el' defines the real thing as it loads.
(defvar vterm-always-compile-module)

(defun my/ensure-vterm-module ()
  "Install `vterm' and build its Emacs module unless the module is present.
Called once at startup, before any module installs a terminal package.  A
missing module is re-checked on every startup, so a machine that lacked
`cmake' or the `libvterm' headers builds as soon as they are installed; the
packages to install are listed under `System packages' in the README.  A
failure only warns, so `M-x vterm' still offers to build the module itself and
reports the toolchain error."
  (unless (locate-library "vterm-module")
    (condition-case err
        (let ((saved (and (boundp 'vterm-always-compile-module)
                          vterm-always-compile-module)))
          (unwind-protect
              (progn
                (unless (package-installed-p 'vterm)
                  (package-install 'vterm))
                ;; Assign rather than `let': vterm.el defcustoms this variable
                ;; while it loads, and a lexical binding of ours would turn that
                ;; definition into an error.  Non-nil builds the module without
                ;; the `y-or-n-p' that reads EOF in a batch Emacs.
                (setq vterm-always-compile-module t)
                ;; Name the common cause up front: `vterm-module-compile' reports
                ;; its own cmake failures too late to be useful here.
                (unless (executable-find "cmake")
                  (error "cmake is not installed"))
                (require 'vterm))
            (setq vterm-always-compile-module saved)))
      (error
       ;; Startup continues; see the docstring for why this is only a warning.
       (display-warning
        'init
        (format (concat "Could not build vterm's native module: %s.  See the "
                        "*Install vterm* buffer; install cmake and the libvterm "
                        "headers, then run M-x vterm-module-compile.")
                (error-message-string err))
        :warning)))))

(my/ensure-vterm-module)

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
