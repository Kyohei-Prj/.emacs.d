;;; init-languages.el --- Language configuration -*- lexical-binding: t; -*-

(require 'eglot)

;; Python: BasedPyright.
(add-to-list 'eglot-server-programs
             '((python-ts-mode python-mode)
               . ("rass"
                  "--"
                  "basedpyright-langserver" "--stdio"
                  "--"
                  "ruff" "server")))
(use-package uv-mode
  :ensure t
  :hook ((python-mode . uv-mode-auto-activate-hook)
         (python-ts-mode . uv-mode-auto-activate-hook)))

(use-package ruff-format
  :ensure t
  :hook ((python-mode . ruff-format-on-save-mode)
         (python-ts-mode . ruff-format-on-save-mode)))

;; JavaScript / TypeScript / TSX.
(with-eval-after-load 'eglot
  (add-to-list
   'eglot-server-programs
   '((tsx-ts-mode typescript-ts-mode js-ts-mode)
	   . ("rass"
		    "--"
		    "typescript-language-server" "--stdio"
		    "--"
		    "tailwindcss-language-server" "--stdio"))))

;; SQL.
(add-to-list 'eglot-server-programs
             '(sql-mode
               . ("sql-language-server" "up" "--method" "stdio")))

;; Markdown.
(add-to-list 'eglot-server-programs
             '(markdown-mode
               . ("marksman" "server")))

;; Dockerfile.
(add-to-list 'eglot-server-programs
             '(dockerfile-mode
               . ("docker-langserver" "--stdio")))

;; Bash.
(add-to-list 'eglot-server-programs
             '(bash-ts-mode
               . ("bash-language-server" "start")))

;; Start LSP in Markdown buffers.
(add-hook 'markdown-mode-hook #'eglot-ensure)

;; Start LSP in SQL buffers.
(add-hook 'sql-mode-hook #'eglot-ensure)

;; Start LSP in Dockerfile buffers.
(add-hook 'dockerfile-mode-hook #'eglot-ensure)

;; Markdown.
(use-package markdown-mode
  :mode
  ("\\.md\\'" . markdown-mode)
  ("\\.mdx\\'" . markdown-mode)
  :custom
  (markdown-command "pandoc")
  (markdown-fontify-code-blocks-natively t))

;; Docker
(use-package which-key
  :ensure nil
  :init
  (which-key-mode 1))

;; Dockerfile.
(use-package dockerfile-mode
  :mode
  ("Dockerfile\\'" . dockerfile-mode)
  ("\\.dockerfile\\'" . dockerfile-mode))

;; JSON.
(add-to-list 'auto-mode-alist
             '("\\.json\\'" . json-ts-mode))

;; SQL.
(add-to-list 'auto-mode-alist
             '("\\.sql\\'" . sql-mode))

;; HTML.
(add-to-list 'eglot-server-programs
             '((html-mode html-ts-mode)
               . ("vscode-html-language-server" "--stdio")))

;; CSS.
(add-to-list 'eglot-server-programs
             '((css-mode css-ts-mode)
               . ("vscode-css-language-server" "--stdio")))

;; JSON.
(add-to-list 'eglot-server-programs
             '((json-mode json-ts-mode)
               . ("vscode-json-language-server" "--stdio")))

;; YAML.
(add-to-list 'eglot-server-programs
             '((yaml-mode yaml-ts-mode)
               . ("yaml-language-server" "--stdio")))

(provide 'init-languages)
;;; init-languages.el ends here
