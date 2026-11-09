;;; init-lsp.el --- Language Server Protocol -*- lexical-binding: t; -*-

(require 'eglot)

;; Start LSP automatically in programming buffers.
(add-hook 'prog-mode-hook #'eglot-ensure)

;; LSP keybindings.
(define-prefix-command 'my/lsp-map)

(global-set-key (kbd "C-c l") 'my/lsp-map)

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

;; Diagnostics.
(setq flymake-no-changes-timeout 0.5
      flymake-start-on-flymake-mode t)

(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (flymake-mode 1)))

;; Display documentation in the echo area.
(setq eldoc-echo-area-use-multiline-p nil)

(provide 'init-lsp)
;;; init-lsp.el ends here
