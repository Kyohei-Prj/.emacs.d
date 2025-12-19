;;; init.el --- Cleaned Configuration for CLI/Web Dev -*- lexical-binding: t; -*-

;; ===========================================================
;; 1. SYSTEM, PACKAGE MANAGER & DEFAULTS
;; ===========================================================
;; --- Garbage Collection (Startup Speed) ---
(setq gc-cons-threshold (* 50 1000 1000))

;; --- Package Management Setup ---
(require 'package)
(setq package-archives
      '(
        ("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;; --- Environment Variables (MacOS/Linux GUI) ---
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))

;; --- Basic UI Cleanup ---
(setq inhibit-startup-message t
      visible-bell t
      use-short-answers t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(column-number-mode)

;; Line Numbers (Global, but disabled in specific modes)
(global-display-line-numbers-mode t)
(dolist (mode
         '(org-mode-hook
           term-mode-hook
           shell-mode-hook
           eshell-mode-hook
           treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; --- Editor Behavior ---
(electric-pair-mode 1) ; Auto-close brackets
(save-place-mode 1) ; Remember cursor position
(windmove-default-keybindings) ; Shift+Arrow to switch windows
(use-package which-key
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0.05))

;; ===========================================================
;; 2. THEME & VISUALS
;; ===========================================================
(use-package doom-themes
  :config (load-theme 'doom-one t))

(use-package nerd-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 25))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ===========================================================
;; 3. NAVIGATION (VERTICO / CONSULT / ORDERLESS)
;; ===========================================================
(use-package vertico
  :init (vertico-mode))

(use-package savehist
  :init (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)))

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

;; ===========================================================
;; 4. IN-BUFFER COMPLETION (CORFU / CAPE)
;; ===========================================================
(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary 'separator)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current nil)
  (corfu-preselect 'first)
  (corfu-scroll-margin 5)
  (corfu-max-width 150)
  (corfu-min-width 50)
  :bind (:map corfu-map ("M-d" . corfu-popupinfo-toggle)))

;; Support for Terminal Modes
(use-package corfu-terminal
  :after corfu
  :config (unless (display-graphic-p)
            (corfu-terminal-mode +1)))

;; Icons in completion menu-bar-mode
(use-package kind-icon
  :if (display-graphic-p)
  :after corfu
  :custom (kind-icon-default-face 'corfu-default)
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Completion backends extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; ===========================================================
;; 5. PROJECT & VERSION CONTROL
;; ===========================================================
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package diff-hl
  :init
  (global-diff-hl-mode +1)
  (global-diff-hl-show-hunk-mouse-mode +1)
  (diff-hl-margin-mode +1)
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode)))

(use-package projectile
  :init (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom (projectile-completion-system 'vertico))

;; ===========================================================
;; 6. LSP & TREESITTER ENGINE
;; ===========================================================

;; --- Global Treesitter ---
(use-package treesit-auto
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; --- Formatting (Apheleia + Prettier) ---
(use-package apheleia
  :config (setf (alist-get 'prettier apheleia-formatters)
                '("prettier" "--stdin-filepath" filepath))
  ;; Use prettier for TS/JS/Web modes
  (dolist (mode '(js-ts-mode
                  typescript-ts-mode
                  tsx-ts-mode
                  css-ts-mode
                  html-ts-mode
                  json-ts-mode))
    (setf (alist-get mode apheleia-mode-alist) 'prettier))
  (apheleia-global-mode +1))

;; --- LSP Configuration ---
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode
           python-ts-mode) . lsp-deferred))
  :init
  ;; Performance Settings
  (setq gc-cons-threshold (* 1024 1024 100))
  (setenv "LSP_USE_PLISTS" "true")
  (setq read-process-output-max (* 1024 1024 32))
  :custom
  (lsp-idle-delay 0.01)
  (lsp-log-io nil)
  (lsp-completion-provider :none) ;; We use Corfu
  (lsp-diagnostics-provider :flymake)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-ui-doc-enable t)
  (lsp-ui-sideline-enable t)
  (lsp-modeline-code-actions-enable t)
  (lsp-eldoc-enable-hover t)
  (lsp-signature-auto-activate t)
  (lsp-signature-render-documentation t))

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t))

;; --- LSP Booster (Performance Hack) ---
;; This advises LSP to use the external `emacs-lsp-booster` tool if available
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)
             (not (file-remote-p default-directory))
             (bound-and-true-p lsp-use-plists)
             (not (functionp 'json-rpc-connection))
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

;; --- Flymake (Diagnostics) ---
(use-package flymake
  :ensure nil
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)
         ("C-c l" . flymake-show-buffer-diagnostics))
  :custom
  (flymake-wrap-around t)
  (flymake-no-changes-timeout 0.5)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Flymake diagnostics"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.25))))

;; ===========================================================
;; 7. LANGUAGE SPECIFIC SETUP
;; ===========================================================
;; --- Frontend ---
(use-package lsp-eslint
  :ensure nil
  :after lsp-mode)

(unless (package-installed-p 'lsp-tailwindcss)
  (package-vc-install "https://github.com/merrickluo/lsp-tailwindcss"))

(use-package lsp-tailwindcss
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config (dolist (tw-major-mode
                   '(css-mode
                     css-ts-mode
                     typescript-mode
                     typescript-ts-mode
                     tsx-ts-mode
                     js2-mode
                     js-ts-mode
                     clojure-mode))
            (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))

(use-package add-node-modules-path
  :hook (tsx-ts-mode-hook . add-node-modules-path))

;; --- Markdown ---
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

;; --- Python ---
(use-package lsp-pyright
  :custom (lsp-pyright-langserver-command "pyright")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package pyvenv
  :config (pyvenv-mode 1))

(use-package uv-mode
  :hook (python-ts-mode . uv-mode-auto-activate-hook))

(use-package ruff-format
  :hook (python-ts-mode-hook . ruff-format-on-save-mode))

(use-package ein
  :custom (ein:output-area-inlined-images t))

;; --- Docker ---
(use-package docker)
(use-package dockerfile-mode :mode "Dockerfile\\'")

;; ===========================================================
;; 8. CUSTOM TAB BAR (C-z)
;; ===========================================================
(use-package tab-bar
  :ensure nil
  :init (tab-bar-mode 1)
  :config (setq tab-bar-show 1
                tab-bar-close-button-show nil
                tab-bar-new-button-show nil
                tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  ;; Custom Tab Name Formatting (0-indexed)
  (setq tab-bar-tab-name-format-function
        (lambda (tab i)
          (let ((face (funcall tab-bar-tab-face-function tab)))
            (propertize
             (format " %d: %s " (- i 1) (alist-get 'name tab))
             'face face))))
  ;; Keymaps
  (define-prefix-command 'my-tab-bar-map)
  (global-set-key (kbd "C-z") 'my-tab-bar-map)
  (define-key my-tab-bar-map (kbd "c") 'tab-bar-new-tab)
  (define-key my-tab-bar-map (kbd "k") 'tab-bar-close-tab)
  ;; Define keys 0-9
  (dotimes (i 10)
    (define-key my-tab-bar-map (kbd (number-to-string i))
                `(lambda () (interactive) (tab-bar-select-tab ,(+ i 1))))))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(add-node-modules-path apheleia blackout cape code-cells
                           corfu-terminal diff-hl docker
                           dockerfile-mode doom-modeline doom-themes
                           ef-themes ein el-get elscreen
                           embark-consult exec-path-from-shell
                           flycheck highlight-indent-guides isend-mode
                           kind-icon leaf-convert leaf-tree lsp-java
                           lsp-pyright lsp-tailwindcss lsp-ui magit
                           marginalia multi-term mwim nerd-icons
                           nodejs-repl orderless poetry projectile
                           rainbow-delimiters ruff-format rustic
                           skewer-mode slime-company swiper
                           treesit-auto uv-mode vertico yasnippet))
 '(which-key-idle-delay 0.5 nil nil "Customized with use-package which-key")
 '(which-key-idle-secondary-delay 0.05 nil nil "Customized with use-package which-key"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
