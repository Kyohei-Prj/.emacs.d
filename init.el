(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (setq package-list '(leaf
                        highlight-indent-guides
                        rainbow-delimiters
                        company-jedi
                        yasnippet
                        flycheck
                        lsp-mode
                        swiper
                        multi-term
                        elscreen
                        which-key
                        magit
                        docker
                        counsel-projectile
                        pyvenv
                        ein
                        dracula-theme
			elpy
			yascroll
			rustic
			neotree))
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


;; personal settings
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode nil)
(set-fringe-mode 10)
(set-face-attribute 'default nil :height 100)
(setq inhibit-startup-message t)
(setq visible-bell t)
(global-linum-mode t)
(windmove-default-keybindings)
(global-set-key (kbd "C-j") 'goto-line)
(load-theme 'dracula t)
(set-face-attribute 'region nil :background "blue")
(recentf-mode 1)


;; Enable company-mode for auto-completion
(leaf company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 1))

;; Configure elpy for Python development
(leaf elpy
  :ensure t
  :config
  (elpy-enable)
  (setq python-shell-interpreter "python3"))

;; Configure jedi for elpy
(leaf elpy
  :config
  (setq elpy-rpc-backend "jedi"))

;; Enable auto-completion in EIN notebooks
(leaf ein
  :ensure t
  :config
  (leaf ein-notebook
    :require t)
  (leaf company-jedi
    :ensure t
    :require t
    :after ein
    :config
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'ein:connect-mode-hook 'company-mode))

;; Configure swiper for buffer and file searching
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
  :config
  (counsel-projectile-mode))

(leaf dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

;; Add Rust language support with rust-analyzer LSP
(leaf rustic
  :ensure t
  :config
  (setq rustic-lsp-server 'rust-analyzer))

(leaf neotree
  :ensure t
  :bind ("M-t" . neotree-toggle))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:output-area-inlined-images t)
 '(package-selected-packages
   '(neotree rustic dracula-theme counsel-projectile docker magit which-key counsel elscreen multi-term yasnippet rainbow-delimiters highlight-indent-guides swiper company-jedi ein elpy blackout el-get hydra leaf-keywords)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
