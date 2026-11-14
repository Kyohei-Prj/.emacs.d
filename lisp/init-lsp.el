;;; init-lsp.el --- Language Server Protocol -*- lexical-binding: t; -*-

(require 'eglot)

;; Start LSP automatically in programming buffers.
(add-hook 'prog-mode-hook #'eglot-ensure)

;; LSP keybindings.  Bound in `eglot-mode-map' (the keymap of
;; `eglot-managed-mode') rather than globally: the commands are meaningless
;; without a server, and keeping the prefix out of the global map leaves
;; `C-c l' available for other packages and user bindings.
(define-prefix-command 'my/lsp-map)

(define-key eglot-mode-map (kbd "C-c l") my/lsp-map)

(define-key my/lsp-map (kbd "r") #'eglot-rename)
(define-key my/lsp-map (kbd "a") #'eglot-code-actions)
(define-key my/lsp-map (kbd "f") #'eglot-format)
(define-key my/lsp-map (kbd "h") #'eldoc)
(define-key my/lsp-map (kbd "d") #'xref-find-definitions)
(define-key my/lsp-map (kbd "D") #'xref-find-references)
(define-key my/lsp-map (kbd "q") #'eglot-shutdown)
(define-key my/lsp-map (kbd "R") #'eglot-reconnect)

(setq eglot-autoshutdown t
      ;; Don't keep a log of LSP traffic (saves memory on chatty servers).
      ;; To debug a server: M-: (setq eglot-events-buffer-config
      ;;                             '(:size 200000 :format full))
      ;;              M-x eglot-reconnect, then M-x eglot-events-buffer.
      eglot-events-buffer-config '(:size 0 :format full)
      eglot-sync-connect nil
      eglot-extend-to-xref t)

;; Diagnostics.  Eglot turns Flymake on in managed buffers itself, so there
;; is no need to hook `eglot-managed-mode' and enable it again.
(setq flymake-no-changes-timeout 0.5
      flymake-start-on-flymake-mode t)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error))

;; Display documentation in the echo area.
(setq eldoc-echo-area-use-multiline-p nil)

(provide 'init-lsp)
;;; init-lsp.el ends here
