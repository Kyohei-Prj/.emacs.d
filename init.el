;;; init.el --- my Emacs configuration:

;;; Commentray:
;; mainly for Python dev

;;; Code:
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

;; productivity utils
(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 0.1))
  :global-minor-mode global-auto-revert-mode)

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

;; general programming utils
(leaf prog-mode
  :hook
  (prog-mode-hook . display-line-numbers-mode))

(leaf code-cells
  :ensure t)

(leaf isend-mode
  :ensure t)

(leaf highlight-indent-guides
  :ensure t
  :hook
  (prog-mode-hook . highlight-indent-guides-mode))

(leaf rainbow-delimiters
  :ensure t
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(leaf tree-sitter
  :ensure t)

(leaf tree-sitter-langs
  :ensure t)

(leaf mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
	 ("C-e" . mwim-end-of-code-or-line)))

(leaf magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(leaf flycheck
  :ensure t
  :hook (prog-mode-hook . flycheck-mode))

(leaf yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(leaf company
  :ensure t
  :hook (prog-mode-hook . company-mode)
  :custom
  (company-minimum-prefix-length . 1))
;; add company-quickhelp

(leaf lsp-mode
  :ensure t)

(leaf lsp-ui
  :ensure t)

(leaf py-autopep8
  :ensure t
  :hook
  (python-mode-hook . py-autopep8-mode))

(leaf python
  :ensure t
  :hook
  (python-mode-hook . code-cells-mode)
  :custom
  (python-indent-guess-indent-offset . nil)
  (python-shell-interpreter . "ipython")
    :config
  (leaf pyvenv
    :ensure t
    :config (pyvenv-mode 1)))

(leaf lsp-pyright
  :ensure t
  :hook (python-mode-hook . (lambda ()
			    (require 'lsp-pyright)
			    (lsp-deferred))))

;; (leaf elpy
;;   :ensure t
;;   :config (elpy-enable))

(leaf ein
  :ensure t)

(leaf rustic
  :ensure t
  :config
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
  (add-hook 'rust-mode-hook 'lsp-deferred))

;; Connect Python script and EIN
(defun mark-region-between-point (start end)
  (interactive "r")
  (push-mark start nil t)
  (goto-char end)
  (activate-mark))

(defun my-ein-workspace-insert-cell-below (contents destination)
  (with-current-buffer destination
    (end-of-buffer)
    (funcall (key-binding (kbd "C-c C-b")))
    (insert contents)
    (funcall (key-binding (kbd "C-c C-c")))
    (message "execute cell")))

(defun my-isend-send ()
  (interactive)
  (isend--check)

  (let* ((region-active (region-active-p))

	 (bds (isend--region-boundaries))
	 (begin (car bds))
	 (end (cdr bds))

	 (origin (current-buffer))
	 (destination (get-buffer isend--command-buffer)))
    (with-temp-buffer
      (insert-buffer-substring origin begin end)
      (let ((contents (buffer-substring-no-properties (point-min) (point-max))))
	(my-ein-workspace-insert-cell-below contents destination))))
  (deactivate-mark))

(defun my-execute-cell (start end)
  (interactive (code-cells--bounds (prefix-numeric-value current-prefix-arg)
				   'use-region
				   'no-header))
  (mark-region-between-point start end)
  (my-isend-send)
  (pulse-momentary-highlight-region start end))

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'my-execute-cell)))

(defun create-code-block ()
  "Insert # %% at the cursor position and another two lines below."
  (interactive)
  (forward-line 1)
  (insert "\n# %%\n\n")
  (forward-line -1)
)

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-b") 'create-code-block)))



(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(seq rustic ein lsp-pyright python py-autopep8 lsp-ui lsp-mode company yasnippet flycheck magit mwim tree-sitter-langs tree-sitter rainbow-delimiters highlight-indent-guides isend-mode code-cells counsel-projectile counsel which-key swiper elscreen multi-term leaf-convert leaf-tree blackout el-get hydra leaf-keywords))
 '(warning-suppress-types '((ein))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
