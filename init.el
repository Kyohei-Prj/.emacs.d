(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (setq package-list '(leaf))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package)))

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


(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode nil)
(set-fringe-mode 10)
(setq inhibit-startup-message t)
(setq visible-bell t)
(windmove-default-keybindings)
(global-set-key (kbd "C-j") 'goto-line)
(global-display-line-numbers-mode)
(recentf-mode 1)

(leaf pyvenv
  :ensure t)

(leaf eglot
  :ensure t
  :hook (python-mode-hook . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(pythom-mode . ("pyright-langserver" "--stdio"))))

(leaf company
  :ensure t
  :hook (prog-mode-hook .company-mode)
  :config
  (setq company-minimum-prefix-length 1))

(leaf flycheck
  :ensure t
  :hook (prog-mode-hook . flycheck-mode))

(leaf python-mode
  :ensure t
  :mode ("\\.py\\'" . python-mode))

(leaf company-jedi
  :ensure t
  :after company)

(leaf ein
  :ensure t
  :config
  (add-hook 'ein:notebook-mode-hook
            (lambda ()
              (setq-local company-backends '(company-jedi)))))

(leaf swiper
  :ensure t
  :bind (("C-s" . swiper))
  :config
  (setq swiper-include-line-number-in-search t))

;; Add packages for better coding experience
(leaf highlight-indent-guides
  :ensure t
  :hook
  (prog-mode . highlight-indent-guides-mode))

(leaf rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(leaf yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(leaf multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/zsh"))

(leaf elscreen
  :ensure t
  :config
  (elscreen-start)
  (setq elscreen-display-tab t))

(leaf counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . counsel-switch-buffer)
         ("C-c r" . counsel-recentf))
  :config
  (setq ivy-initial-inputs-alist nil))

(leaf which-key
  :ensure t
  :config
  (which-key-mode))

(leaf magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(leaf docker
  :ensure t)

(leaf counsel-projectile
  :ensure t
  :bind(("C-x p C-f" . 'counsel-projectile-find-file))
  :config
  (counsel-projectile-mode))

(leaf dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

(leaf rustic
  :ensure t
  :config
  (setq rustic-lsp-server 'rust-analyzer))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(leaf)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:basecell-input-area-face ((t (:extend t :background "color-234")))))
