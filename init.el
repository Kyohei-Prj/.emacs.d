;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>

;; leaf utilities
(leaf leaf-tree :ensure t)
(leaf leaf-convert :ensure t)

;; configure builtin
(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :config
  (load-theme 'modus-vivendi t)
  (windmove-default-keybindings)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (global-auto-revert-mode 1)
  (electric-pair-mode 1)
  :custom
  (inhibit-startup-message . t)
  (visible-bell . t)
  (use-dialog-box . nil)
  (history-lenght . 25)
  (global-auto-revert-non-file-buffers . t)
  (modus-themes-mode-line . '(accented borderless))
  (modus-themes-region . '(bg-only))
  (modus-themes-completions . 'opinionated)
  (modus-themes-bold-constructs . t)
  (modus-themes-italic-constructs . t)
  (modus-themes-paren-match . '(bold intense))
  (modus-themes-syntax . '(yellow-comments)))

(leaf prog-mode
  :hook
  (prog-mode-hook . display-line-numbers-mode))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 0.1))
  :global-minor-mode global-auto-revert-mode)

(leaf highlight-indent-guides
  :ensure t
  :hook
  (prog-mode . highlight-indent-guides-mode))

(leaf rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(leaf multi-term
  :ensure t
  :custom
  (multi-term-program . "/bin/zsh"))

(leaf elscreen
  :ensure t
  :config
  (elscreen-start)
  :custom
  (elscreen-display-tab . t))

(leaf swiper
  :ensure t
  :bind (("C-s" . swiper))
  :custom
  (swiper-include-line-number-in-search . t))

(leaf which-key
  :ensure t
  :config
  (which-key-mode))

(leaf magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(leaf counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . counsel-switch-buffer)
         ("C-c r" . counsel-recentf))
  :custom
  (ivy-initial-inputs-alist . nil))

(leaf counsel-projectile
  :ensure t
  :bind(("C-x p C-f" . 'counsel-projectile-find-file))
  :config
  (counsel-projectile-mode))

(leaf company
  :ensure t
  :hook (prog-mode-hook . company-mode)
  :custom
  (company-minimum-prefix-length . 1))

;; add company-quickhelp

(leaf flycheck
  :ensure t
  :hook (prog-mode-hook . flycheck-mode))

(leaf python
  :ensure t
  :config
  (leaf pyvenv
    :ensure t
    :config (pyvenv-mode 1)))

(leaf elpy
  :ensure t
  :config (elpy-enable))

(leaf ein
  :ensure t)

(leaf rustic
  :ensure t
  :config
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
  (add-hook 'rust-mode-hook 'lsp-deferred))

(leaf yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(provide 'init)
;;; init.el ends here
