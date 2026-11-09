;;; init-languages.el --- Language configuration -*- lexical-binding: t; -*-

(require 'eglot)

;;; Multi-server aggregation.
;;
;; `rass' (uv tool install rassumfrassum) multiplexes several language
;; servers behind a single LSP endpoint, so one Eglot connection can
;; talk to e.g. basedpyright *and* ruff.  When `rass' is not installed,
;; or only one of the servers is available, fall back to that single
;; server instead of failing to start.

(defun my/rass-command (servers)
  "Return an Eglot CONTACT aggregating SERVERS.
SERVERS is a list of (PROGRAM [ARGS...]) lists, best server first.
Entries whose PROGRAM is not on `exec-path' are dropped.  Several
survivors are wrapped in a `rass' invocation, a single survivor is
returned as-is, and when nothing is installed the first entry is
returned so Eglot reports a clear \"program not found\" error."
  (let ((available
         (seq-filter (lambda (server) (executable-find (car server)))
                     servers)))
    (cond
     ((and (cdr available) (executable-find "rass"))
      (let ((args nil))
        (dolist (server available)
          (when args
            (setq args (append args '("--"))))
          (setq args (append args server)))
        (cons "rass" (cons "--" args))))
     (available (car available))
     (t (car servers)))))

;;; Python.

(add-to-list 'eglot-server-programs
             (cons '(python-ts-mode python-mode)
                   ;; BasedPyright for typing, Ruff for linting.
                   (lambda (&rest _)
                     (my/rass-command
                      '(("basedpyright-langserver" "--stdio")
                        ("ruff" "server"))))))

(defun my/uv-mode-activate ()
  "Activate the project's uv virtualenv in the current buffer.

Unlike `uv-mode-auto-activate-hook', which only fires in buffers
derived from `python-mode' and therefore never runs in the
`python-ts-mode' buffers that `.py' files open in, this works in
both.  Requires a `.venv' in (or above) the current directory, and
only loads `uv-mode' once one is found."
  (when (and (executable-find "uv")
             (locate-dominating-file default-directory ".venv"))
    (require 'uv-mode)
    (uv-mode 1)
    (uv-mode-set)))

(use-package uv-mode
  :ensure t
  :functions (uv-mode-set)
  :hook ((python-mode . my/uv-mode-activate)
         (python-ts-mode . my/uv-mode-activate)))

(use-package ruff-format
  :ensure t
  :hook ((python-mode . ruff-format-on-save-mode)
         (python-ts-mode . ruff-format-on-save-mode)))

;;; JavaScript / TypeScript / TSX.

(defun my/tsc-supports-lsp-p ()
  "Return non-nil if the `tsc' on `exec-path' understands `--lsp'.
The native compiler shipped with TypeScript 7 added `--lsp`; older
releases need `typescript-language-server' instead."
  (when-let* ((tsc (executable-find "tsc"))
              (banner (car (ignore-errors (process-lines tsc "--version")))))
    (and (string-match "\\`Version \\([0-9]+\\)" banner)
         (>= (string-to-number (match-string 1 banner)) 7))))

(add-to-list 'eglot-server-programs
             (cons '(tsx-ts-mode typescript-ts-mode js-ts-mode)
                   (lambda (&rest _)
                     (my/rass-command
                      (list (if (my/tsc-supports-lsp-p)
                                '("tsc" "--lsp" "--stdio")
                              '("typescript-language-server" "--stdio"))
                            '("tailwindcss-language-server" "--stdio"))))))

;;; Other languages.

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

;; Start LSP in Markdown buffers.
(add-hook 'markdown-mode-hook #'eglot-ensure)

;; Start LSP in SQL buffers.
(add-hook 'sql-mode-hook #'eglot-ensure)

;; Start LSP in Dockerfile buffers.
(add-hook 'dockerfile-mode-hook #'eglot-ensure)

;;; Major modes.

;; Markdown.
(use-package markdown-mode
  :mode
  ("\\.md\\'" . markdown-mode)
  ("\\.mdx\\'" . markdown-mode)
  :custom
  (markdown-command "pandoc")
  (markdown-fontify-code-blocks-natively t))

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

(provide 'init-languages)
;;; init-languages.el ends here
