;;; init.el --- Modern Emacs configuration -*- coding: utf-8 ; lexical-binding: t -*-

;;; Commentary:
;; A modern, clean Emacs configuration focused on Python development
;; with support for multiple languages and productivity enhancements.

;;; Code:

;;;; Performance and Startup Optimization
(defvar my/gc-cons-threshold-backup gc-cons-threshold
  "Backup of the original gc-cons-threshold value.")

(defvar my/file-name-handler-alist-backup file-name-handler-alist
  "Backup of the original file-name-handler-alist value.")

;; Optimize startup performance
(setq gc-cons-threshold (* 50 1000 1000))
(setq file-name-handler-alist nil)

;; LSP performance settings
(setenv "LSP_USE_PLISTS" "true")
(setq read-process-output-max (* 1024 1024 16)) ; 16MB

;; Restore GC settings after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold my/gc-cons-threshold-backup
                  file-name-handler-alist my/file-name-handler-alist-backup)))

;;;; Package Management Setup
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org" . "https://orgmode.org/elpa/")
                       ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    :config
    (leaf-keywords-init)))

;; Leaf utilities for development
(leaf leaf-tree :ensure t)
(leaf leaf-convert :ensure t)

;;;; Core Emacs Configuration
(leaf cus-start
  :doc "Configure built-in Emacs settings"
  :tag "builtin" "internal"
  :config
  ;; UI improvements
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode 1)
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "color-233")
  (set-face-foreground 'highlight nil)
  (show-paren-mode 1)
  
  ;; Navigation and window management
  (windmove-default-keybindings)
  
  ;; File and history management
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (global-auto-revert-mode 1)
  
  ;; Editing enhancements
  (electric-pair-mode 1)
  (delete-selection-mode 1)
  
  :custom
  ;; Startup
  (inhibit-startup-message . t)
  (initial-scratch-message . "")
  
  ;; UI
  (visible-bell . t)
  (use-dialog-box . nil)
  (ring-bell-function . 'ignore)
  
  ;; Files and backups
  (backup-directory-alist . `(("." . ,(expand-file-name "backups" user-emacs-directory))))
  (auto-save-file-name-transforms . `((".*" ,(expand-file-name "auto-save" user-emacs-directory) t)))
  (create-lockfiles . nil)
  
  ;; History
  (history-length . 1000)
  (history-delete-duplicates . t)
  
  ;; Auto-revert
  (global-auto-revert-non-file-buffers . t)
  (auto-revert-verbose . nil)
  (auto-revert-interval . 0.1)
  
  ;; Package management
  (package-install-upgrade-built-in . t)
  
  ;; Compilation
  (compilation-scroll-output . t)
  
  ;; Indentation
  (tab-width . 4)
  (indent-tabs-mode . nil))

;;;; Theme and Appearance
(leaf ef-themes
  :ensure t
  :config
  (ef-themes-select 'ef-dark)
  (put 'ef-themes-select 'safe-local-variable #'symbolp))

;;;; Modern Completion Framework
(leaf vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle . t)
  (vertico-resize . t)
  (vertico-count . 20))

(leaf orderless
  :ensure t
  :custom
  (completion-styles . '(orderless basic))
  (completion-category-defaults . nil)
  (completion-category-overrides . '((file (styles partial-completion)))))

(leaf marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

(leaf consult
  :ensure t
  :bind (;; C-c bindings in mode-specific-map
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ;; C-x bindings in ctl-x-map
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ("C-x C-r" . consult-recent-file)
         ;; M-g bindings in goto-map
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in search-map
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         (:isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi))
         ;; Minibuffer history
         (:minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history)))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(leaf embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(leaf embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;; Productivity Utilities
(leaf which-key
  :ensure t
  :blackout
  :config
  (which-key-mode)
  :custom
  (which-key-idle-delay . 0.5)
  (which-key-idle-secondary-delay . 0.05))

(leaf exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(leaf multi-term
  :ensure t
  :custom
  (multi-term-program . "/bin/zsh")
  (multi-term-switch-after-close . 'PREVIOUS)
  :hook
  (term-mode-hook . (lambda ()
                      (setq-local global-hl-line-mode nil)
                      (hl-line-mode -1))))


(leaf elscreen
  :ensure t
  :config
  (elscreen-start)
  :custom
  (elscreen-display-tab . t)
  (elscreen-tab-display-control . nil))

(leaf swiper
  :ensure t
  :bind (("C-s" . swiper))
  :custom
  (swiper-include-line-number-in-search . t))

;;;; General Programming Support
(leaf prog-mode
  :hook
  (prog-mode-hook . display-line-numbers-mode)
  (prog-mode-hook . hs-minor-mode))

(leaf highlight-indent-guides
  :ensure t
  :hook (prog-mode-hook . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method . 'character)
  (highlight-indent-guides-character . ?\|)
  (highlight-indent-guides-responsive . 'top))

(leaf rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(leaf treesit
  :mode (("\\.tsx\\'"       . tsx-ts-mode)
         ("\\.js\\'"        . tsx-ts-mode)
         ("\\.mjs\\'"       . typescript-ts-mode)
         ("\\.mts\\'"       . typescript-ts-mode)
         ("\\.cjs\\'"       . typescript-ts-mode)
         ("\\.ts\\'"        . tsx-ts-mode)
         ("\\.jsx\\'"       . tsx-ts-mode)
         ("\\.css\\'"       . css-ts-mode)
         ("\\.json\\'"      . json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'"    . prisma-ts-mode)
         ("\\.py\\'"    . python-ts-mode))
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (html . ("https://github.com/tree-sitter/tree-sitter-html"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python"))
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (make "https://github.com/alemuller/tree-sitter-make")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install grammar if it is missing
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  :config
  (setq treesit-font-lock-level 4)
  ;; Major mode remapping
  (dolist (mapping
           '((python-mode     . python-ts-mode)
             (css-mode        . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode         . typescript-ts-mode)
             (js2-mode        . typescript-ts-mode)
             (c-mode          . c-ts-mode)
             (c++-mode        . c++-ts-mode)
             (c-or-c++-mode   . c-or-c++-ts-mode)
             (bash-mode       . bash-ts-mode)
             (css-mode        . css-ts-mode)
             (json-mode       . json-ts-mode)
             (js-json-mode    . json-ts-mode)
             (sh-mode         . bash-ts-mode)
             (sh-base-mode    . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  ;; Install grammars on startup
  (os/setup-install-grammars))

(leaf mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

;;;; Version Control
(leaf magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :custom
  (magit-diff-refine-hunk . t)
  (magit-repository-directories . '(("~/code" . 2))))

(leaf diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode))

;;;; Project Management
(leaf projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:projectile-command-map
              ("s r" . consult-ripgrep))
  :custom
  (projectile-completion-system . 'default)
  (projectile-enable-caching . t)
  (projectile-indexing-method . 'alien))

;;;; Syntax Checking
(leaf flycheck
  :ensure t
  :hook (prog-mode-hook . flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically . '(save idle-change mode-enabled))
  (flycheck-idle-change-delay . 0.8))

;;;; Snippets
(leaf yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  :custom
  (yas-snippet-dirs . '(yas-installed-snippets-dir)))

;;;; In-Buffer Completion
(leaf corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle . t)
  (corfu-auto . t)
  (corfu-auto-prefix . 1)
  (corfu-auto-delay . 0.1)
  (corfu-quit-at-boundary . 'separator)
  (corfu-quit-no-match . 'separator)
  (corfu-preview-current . nil)
  (corfu-preselect . 'first)
  (corfu-on-exact-match . nil)
  (corfu-scroll-margin . 5)
  (corfu-max-width . 150)
  (corfu-min-width . 50)
  (corfu-popupinfo-mode . t)
  :bind
  (:corfu-map
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("TAB" . corfu-complete)
        ([tab] . corfu-complete)
        ("RET" . corfu-complete)
        ([return] . corfu-complete)))

(leaf corfu-terminal
  :unless (display-graphic-p)
  :after corfu
  :ensure t
  :config
  (corfu-terminal-mode t))

(leaf cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  :bind
  (("C-c p p" . completion-at-point)
   ("C-c p t" . complete-tag)
   ("C-c p d" . cape-dabbrev)
   ("C-c p h" . cape-history)
   ("C-c p f" . cape-file)
   ("C-c p k" . cape-keyword)
   ("C-c p s" . cape-elisp-symbol)
   ("C-c p e" . cape-elisp-block)
   ("C-c p a" . cape-abbrev)
   ("C-c p l" . cape-line)
   ("C-c p w" . cape-dict)
   ("C-c p :" . cape-emoji)
   ("C-c p \\" . cape-tex)))

;;;; LSP Configuration
(leaf lsp-mode
  :ensure t
  :custom
  (lsp-idle-delay . 0.01)
  (lsp-log-io . nil)
  (lsp-completion-provider . :none)
  (lsp-headerline-breadcrumb-enable . t)
  (lsp-enable-symbol-highlighting . t)
  (lsp-ui-doc-enable . t)
  (lsp-ui-sideline-enable . t)
  (lsp-modeline-code-actions-enable . t)
  (lsp-eldoc-enable-hover . t)
  (lsp-signature-auto-activate . t)
  (lsp-signature-render-documentation . t)
  (lsp-semantic-tokens-enable . t))

(leaf lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable . t)
  (lsp-ui-doc-position . 'bottom)
  (lsp-ui-doc-delay . 0.2)
  (lsp-ui-sideline-enable . t)
  (lsp-ui-sideline-show-code-actions . t)
  (lsp-ui-sideline-show-hover . t)
  (lsp-ui-sideline-delay . 0.05)
  (lsp-ui-peek-enable . t)
  (lsp-ui-peek-show-directory . t))

;;;; LSP Booster Configuration
(defun my/lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(defun my/lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)
             (not (file-remote-p default-directory))
             lsp-use-plists
             (not (functionp 'json-rpc-connection))
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'my/lsp-booster--advice-json-parse)

(advice-add 'lsp-resolve-final-command :around #'my/lsp-booster--advice-final-command)

;;;; Python Development
(leaf python
  :ensure t
  :hook
  (python-ts-mode-hook . code-cells-mode)
  (python-ts-mode-hook . (lambda ()
                        (setq-local tab-width 4)
                        (setq-local indent-tabs-mode nil)))
  :custom
  (python-indent-guess-indent-offset . nil)
  (python-indent-offset . 4)
  (python-shell-interpreter . "ipython")
  (python-shell-interpreter-args . "-i --simple-prompt"))

(leaf code-cells
  :ensure t)

(leaf isend-mode
  :ensure t)

(leaf pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))

(leaf uv-mode
  :ensure t
  :hook
  (python-ts-mode-hook . uv-mode-auto-activate-hook))

(leaf ruff-format
  :ensure t
  :hook
  (python-ts-mode-hook . ruff-format-on-save-mode))

(leaf lsp-pyright
  :ensure t
  :hook (python-ts-mode-hook . (lambda ()
                              (require 'lsp-pyright)
                              (lsp-deferred))))

(leaf poetry
  :ensure t
  :bind (("C-c C-p" . poetry)))

(leaf ein
  :ensure t
  :custom
  (ein:output-area-inlined-images . t))

;;;; Python Cell Execution Functions
(defun my/mark-region-between-point (start end)
  "Mark region between START and END."
  (interactive "r")
  (push-mark start nil t)
  (goto-char end)
  (activate-mark))

(defun my/ein-workspace-insert-cell-below (contents destination)
  "Insert CONTENTS into a new cell below in DESTINATION buffer."
  (with-current-buffer destination
    (end-of-buffer)
    (funcall (key-binding (kbd "C-c C-b")))
    (insert contents)
    (funcall (key-binding (kbd "C-c C-c")))
    (message "Executed cell")))

(defun my/isend-send ()
  "Send region to EIN notebook."
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
        (my/ein-workspace-insert-cell-below contents destination))))
  (deactivate-mark))

(defun my/execute-cell (start end)
  "Execute Python cell between START and END."
  (interactive (code-cells--bounds (prefix-numeric-value current-prefix-arg)
                                   'use-region
                                   'no-header))
  (my/mark-region-between-point start end)
  (my/isend-send)
  (pulse-momentary-highlight-region start end))

(defun my/create-code-block ()
  "Insert # %% cell separator."
  (interactive)
  (forward-line 1)
  (insert "\n# %%\n\n")
  (forward-line -1))

(add-hook 'python-ts-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'my/execute-cell)
            (local-set-key (kbd "C-c C-b") 'my/create-code-block)))

;;;; Rust Development
(leaf rustic
  :ensure t
  :config
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
  :hook
  (rustic-mode-hook . lsp-deferred)
  :custom
  (rustic-format-on-save . t)
  (rustic-lsp-client . 'lsp-mode))

;;;; Frontend Development
(leaf typescript-ts-mode
  :hook
  (typescript-ts-mode-hook . lsp-deferred))

(leaf tsx-ts-mode
  :hook
  (tsx-ts-mode-hook . lsp-deferred))

(leaf lsp-eslint
  :after lsp-mode)

(leaf lsp-tailwindcss
  :ensure t
  :hook
  (css-ts-mode-hook . lsp-deferred))

(leaf lsp-tailwindcss
  :ensure t
  :after lsp-mode
  :init
  ;; enable add-on mode (note singular name)
  (setq lsp-tailwindcss-add-on-mode t)
  ;; optionally restrict/extend which major-modes will trigger it
  (setq lsp-tailwindcss-major-modes
        '(rjsx-mode web-mode html-mode css-mode
                    typescript-mode typescript-tsx-mode typescript-ts-mode
                    tsx-ts-mode js-mode js-jsx-mode js-ts-mode))
  ;; if your project doesn't have tailwind.config.js and you still want it
  ;; to start, you can skip the config check (use only if you know what you do)
  (setq lsp-tailwindcss-skip-config-check t)
  :hook ((typescript-ts-mode-hook . lsp-deferred)
         (tsx-ts-mode-hook . lsp-deferred)
         (js-ts-mode-hook . lsp-deferred)
         (js-mode-hook . lsp-deferred)
         (css-mode-hook . lsp-deferred)
         (html-mode-hook . lsp-deferred)))


(leaf css-ts-mode
  :config
  (setq css-ts-offset-level  2))

(leaf add-node-modules-path
  :ensure t
  :hook
  (tsx-ts-mode-hook . add-node-modules-path))

(leaf nodejs-repl
  :ensure t
  :bind (("C-c C-e" . nodejs-repl-send-line)
         ("C-c C-r" . nodejs-repl-send-region)))

(leaf skewer-mode
  :ensure t
  :hook
  (tsx-ts-mode-hook . skewer-mode))

;;;; Java Development
(leaf lsp-java
  :ensure t
  :hook
  (java-mode-hook . lsp-deferred))

;;;; Lisp Development
(leaf slime
  :ensure t
  :config
  (slime-setup '(slime-fancy slime-company))
  :custom
  (inferior-lisp-program . "sbcl")
  (slime-contribs . '(slime-fancy)))

(leaf slime-company
  :ensure t
  :after slime)

;;;; Container Support
(leaf docker
  :ensure t)

(leaf dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

;;;; Final Configuration
(provide 'init)
;;; init.el ends here
