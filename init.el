;; init.el --- My init.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My init.el.

;;; Code:

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/{{pkg}}/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (setq package-list '(leaf
                        blacken
                        highlight-indent-guides
                        rainbow-delimiters
                        company
                        yasnippet
                        flycheck
                        lsp-mode
                        lsp-pyright
                        swiper
                        multi-term
                        elscreen
                        counsel
                        which-key
                        magit
                        docker
                        ivy
                        ivy-rich
                        counsel-projectile
                        pyvenv
                        ein
                        company-c-headers
                        dracula-theme
			exec-path-from-shell
			elpy
			monokai-theme
			yascroll
			ccls
			rustic
			lsp-java))
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


(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf blacken
  :doc "Reformat python buffers using the \"black\" formatter"
  :req "emacs-25.2"
  :tag "emacs>=25.2"
  :url "https://github.com/proofit404/blacken"
  :added "2022-01-09"
  :emacs>= 25.2
  :ensure t
  :custom ((blacken-line-length . 119)
           (blacken-skip-string-normalization . t)))

(leaf highlight-indent-guides
  :ensure t
  :blackout t
  :hook (((prog-mode-hook yaml-mode-hook) . highlight-indent-guides-mode))
  :custom ((highlight-indent-guides-method . 'character) ;; fill,column,character
           (highlight-indent-guides-auto-enabled . t) ;; automatically calculate faces.
           (highlight-indent-guides-responsive . t)
           (highlight-indent-guides-character . ?\|)))

(leaf rainbow-delimiters
  :doc "Highlight brackets according to their depth"
  :tag "tools" "lisp" "convenience" "faces"
  :url "https://github.com/Fanael/rainbow-delimiters"
  :added "2022-01-09"
  :ensure t
  :hook
  ((prog-mode-hook . rainbow-delimiters-mode)))

(leaf company
  :ensure t
  :leaf-defer nil
  :blackout company-mode
  :bind ((company-active-map ("M-n" . nil)
                             ("M-p" . nil)
                             ("C-s" . company-filter-candidates)
                             ("C-n" . company-select-next)
                             ("C-p" . company-select-previous)
                             ("C-i" . company-complete-common-or-cycle))
         (company-search-map ("C-n" . company-select-next)
                             ("C-p" . company-select-previous)))
  :custom ((company-tooltip-limit             . 12)
           (company-idle-delay                . 0)
           (company-minimum-prefix-length     . 1)
	   (company-transformers              . '(company-sort-by-backend-importance))
           (global-company-mode               . t)
           (company-selection-wrap-around     . t)
           (vompany-tooltip-align-annotations . t))
  :config (leaf company-prescient
            :ensure t
            :custom ((company-prescient-mode . t)))
  (leaf company-quickhelp
    :when (display-graphic-p)
    :ensure t
    :custom ((company-quickhelp-delay . 0.8)
             (company-quickhelp-mode  . t))
    :bind (company-active-map ("M-h" . company-quickhelp-manual-begin))
    :hook ((company-mode-hook . company-quickhelp-mode)))
  (leaf company-math
    :ensure t
    :defvar (company-backends)
    :preface (defun c/latex-mode-setup ()
               (setq-local company-backends (append '((company-math-symbols-latex
                                                       company-math-symbols-unicode
                                                       company-latex-commands)) company-backends)))
    :hook ((org-mode-hook . c/latex-mode-setup)
           (tex-mode-hook . c/latex-mode-setup)))
  (leaf yasnippet
    :ensure t
    :blackout yas-minor-mode
    :custom ((yas-indent-line . 'fixed)
             (yas-global-mode . t))
    :bind ((yas-keymap ("<tab>" . nil)) ; conflict with company
           (yas-minor-mode-map ("C-c y i" . yas-insert-snippet)
                               ("C-c y n" . yas-new-snippet)
                               ("C-c y v" . yas-visit-snippet-file)
                               ("C-c y l" . yas-describe-tables)
                               ("C-c y g" . yas-reload-all)))
    :config (leaf yasnippet-snippets
                :ensure t)
      (leaf yatemplate
        :ensure t
        :config (yatemplate-fill-alist))
      (defvar company-mode/enable-yas t
        "Enable yasnippet for all backends.")
      (defun company-mode/backend-with-yas (backend)
        (if (or (not company-mode/enable-yas)
                (and (listp backend)
                     (member 'company-yasnippet backend))) backend (append (if (consp backend) backend (list backend))
                                  '(:with company-yasnippet))))
      (defun set-yas-as-company-backend ()
        (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))
      :hook ((company-mode-hook . set-yas-as-company-backend))))

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :added "2022-01-09"
  :emacs>= 24.3
  :ensure t
  :hook (prog-mode-hook . flycheck-mode)
  :custom ((flycheck-display-errors-delay . 0.3)
           (flycheck-indication-mode . 'left-margin)) ;terminalで使うので、fringeではなくmarginに警告を表示
  :config (add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode) ; flycheckのみでmarginを使用
    (leaf flycheck-inline
      :ensure t
      :hook (flycheck-mode-hook . flycheck-inline-mode)))

(leaf lsp-mode
  :doc "LSP mode"
  :req "emacs-26.1" "dash-2.18.0" "f-0.20.0" "ht-2.3" "spinner-1.7.3" "markdown-mode-2.3" "lv-0"
  :tag "languages" "emacs>=26.1"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :added "2022-01-09"
  :emacs>= 26.1
  :ensure t
  :commands (lsp lsp-deferred)
  :config
  :custom ((lsp-keymap-prefix                  . "C-c l")
           (lsp-log-io                         . nil)
           (lsp-keep-workspace-alive           . nil)
           (lsp-document-sync-method           . 2)
           (lsp-response-timeout               . 5)
           (lsp-enable-file-watchers           . nil)
	   (lsp-print-io                       . nil))
  :hook (lsp-mode-hook . lsp-headerline-breadcrumb-mode)
  :init (leaf lsp-ui
          :ensure t
          :after lsp-mode
          :custom ((lsp-ui-doc-enable            . t)
                   (lsp-ui-doc-position          . 'at-point)
                   (lsp-ui-doc-header            . t)
                   (lsp-ui-doc-include-signature . t)
                   (lsp-ui-doc-max-width         . 150)
                   (lsp-ui-doc-max-height        . 30)
                   (lsp-ui-doc-use-childframe    . nil)
                   (lsp-ui-doc-use-webkit        . nil)
                   (lsp-ui-peek-enable           . t)
                   (lsp-ui-peek-peek-height      . 20)
                   (lsp-ui-peek-list-width       . 50))
          :bind ((lsp-ui-mode-map ([remap xref-find-definitions] .
                                   lsp-ui-peek-find-definitions)
                                  ([remap xref-find-references] .
                                   lsp-ui-peek-find-references))
                 (lsp-mode-map ("C-c s" . lsp-ui-sideline-mode)
                               ("C-c d" . lsp-ui-doc-mode)))
          :hook ((lsp-mode-hook . lsp-ui-mode))))

(leaf lsp-java
  :doc "Java support for lsp-mode"
  :req "emacs-25.1" "lsp-mode-6.0" "markdown-mode-2.3" "dash-2.18.0" "f-0.20.0" "ht-2.0" "request-0.3.0" "treemacs-2.5" "dap-mode-0.5"
  :tag "tools" "languague" "emacs>=25.1"
  :url "https://github.com/emacs-lsp/lsp-java"
  :added "2022-03-12"
  :emacs>= 25.1
  :ensure t
  :require t
  :custom (lsp-java-format-on-type-enabled . nil)
  :hook (java-mode-hook . lsp))

(defun java-getter-setter ()
  (interactive)
  (save-excursion
    (lsp-java-generate-getters-and-setters)
    (c-indent-defun)))
(global-set-key (kbd "C-c C-j C-s") 'java-getter-setter)

(defun java-generate-overrides ()
  (interactive)
  (save-excursion
    (lsp-java-generate-overrides)
    (c-indent-defun)))
(global-set-key (kbd "C-c C-j C-o") 'java-generate-overrides)

(leaf elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (remove-hook 'elpy-modules 'elpy-module-highlight-indentation) ;; インデントハイライトの無効化
  (remove-hook 'elpy-modules 'elpy-module-flymake) ;; flymakeの無効化
  :custom
  (elpy-rpc-python-command . "python3") ;; https://mako-note.com/ja/elpy-rpc-python-version/の問題を回避するための設定
  (flycheck-python-flake8-executable . "flake8")
  :bind (elpy-mode-map
         ("C-c C-r f" . elpy-format-code))
  :hook ((elpy-mode-hook . flycheck-mode)))

;; (leaf lsp-pyright
;;   :doc "Python LSP client using Pyright"
;;   :req "emacs-26.1" "lsp-mode-7.0" "dash-2.18.0" "ht-2.0"
;;   :tag "lsp" "tools" "languages" "emacs>=26.1"
;;   :url "https://github.com/emacs-lsp/lsp-pyright"
;;   :added "2022-03-13"
;;   :emacs>= 26.1
;;   :ensure t
;;   :require t
;;   :hook (python-mode-hook . lsp))

;; (leaf ein
;;   :doc "Emacs IPython Notebook"
;;   :req "emacs-25" "websocket-1.12" "anaphora-1.0.4" "request-0.3.3" "deferred-0.5" "polymode-0.2.2" "dash-2.13.0" "with-editor-0.-1"
;;   :tag "reproducible research" "literate programming" "jupyter" "emacs>=25"
;;   :url "https://github.com/dickmao/emacs-ipython-notebook"
;;   :added "2022-01-10"
;;   :emacs>= 25
;;   :ensure t
;;   :custom
;;   :hook (ein:ipynb-mode-hook . lsp)
;;   (ein:output-area-inlined-images . t))

(leaf company-c-headers
  :doc "Company mode backend for C/C++ header files"
  :req "emacs-24.1" "company-0.8"
  :tag "company" "development" "emacs>=24.1"
  :added "2020-03-25"
  :emacs>= 24.1
  :ensure t
  :after company
  :defvar company-backends
  :config
  (add-to-list 'company-backends 'company-c-headers))

(leaf ccls
  :doc "ccls client for lsp-mode"
  :req "emacs-25.1" "lsp-mode-6.3.1" "dash-2.14.1"
  :tag "c++" "lsp" "languages" "emacs>=25.1"
  :url "https://github.com/MaskRay/emacs-ccls"
  :added "2022-01-15"
  :emacs>= 25.1
  :ensure t
  :custom (ccls-executable . "/usr/bin/ccls")
  :hook ((c-mode c++-mode objc-mode) . (lambda () (require 'ccls) (lsp-deferred)))
  :after lsp-mode)

(leaf elscreen
  :doc "Emacs window session manager"
  :req "emacs-24"
  :tag "convenience" "window" "emacs>=24"
  :url "https://github.com/knu/elscreen"
  :added "2022-01-09"
  :emacs>= 24
  :ensure t
  :hook (after-init-hook . elscreen-start))

(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :blackout t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-use-selectable-prompt . t))
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper)))

  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :blackout t
    :bind (("C-S-s" . counsel-imenu)
           ("C-x C-r" . counsel-recentf)
           ("M-x" . 'counsel-M-x)
           ("C-x b" . 'counsel-ibuffer)
           ("C-x C-f" . 'counsel-find-file)
           ("C-c C-p C-f" . 'counsel-projectile-find-file))
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t))

(leaf ivy-rich
  :doc "More friendly display transformer for ivy."
  :req "emacs-24.5" "ivy-0.8.0"
  :tag "ivy" "emacs>=24.5"
  :emacs>= 24.5
  :ensure t
  :after ivy
  :global-minor-mode t)

(leaf which-key
  :doc "Display available keybindings in popup"
  :req "emacs-24.4"
  :tag "emacs>=24.4"
  :url "https://github.com/justbur/emacs-which-key"
  :added "2022-01-09"
  :emacs>= 24.4
  :ensure t
  :custom ((which-key-idel-delay . 1)
           (which-key-mode . t)))

(leaf multi-term
  :doc "Managing multiple terminal buffers in Emacs."
  :tag "multiple buffer" "terminal" "term"
  :url "http://www.emacswiki.org/emacs/download/multi-term.el"
  :added "2022-01-09"
  :ensure t
  :custom (multi-term-program . "/bin/zsh"))

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-25.1" "dash-20210826" "git-commit-20211004" "magit-section-20211004" "transient-20210920" "with-editor-20211001"
  :tag "vc" "tools" "git" "emacs>=25.1"
  :url "https://github.com/magit/magit"
  :added "2022-01-09"
  :emacs>= 25.1
  :ensure t
  :bind ("C-x g" . magit-status)
  )

(leaf yascroll
  :doc "Yet Another Scroll Bar Mode"
  :req "emacs-26.1"
  :tag "convenience" "emacs>=26.1"
  :url "https://github.com/emacsorphanage/yascroll"
  :added "2022-01-10"
  :emacs>= 26.1
  :ensure t
  :custom(global-yascroll-bar-mode . 1))

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-archives
   '(("org" . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")))
 '(package-selected-packages
   '(java-imports java-snippets javadoc-lookup lsp-java rustic ccls yascroll ivy lsp-ui flycheck-inline yatemplate yasnippet-snippets company-math company-quickhelp company-prescient macrostep leaf-tree leaf-convert blackout el-get hydra leaf-keywords dracula-theme company-c-headers ein pyvenv counsel-projectile ivy-rich docker magit which-key counsel elscreen multi-term swiper lsp-pyright lsp-mode flycheck yasnippet company rainbow-delimiters highlight-indent-guides blacken leaf)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
