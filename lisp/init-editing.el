;;; init-editing.el --- Editing improvements -*- lexical-binding: t; -*-

;; Multiple cursors.
(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

;; Visual undo.
(use-package vundo
  :bind
  ("C-x u" . vundo))

;; Project-specific environment variables.
(use-package envrc
  :hook
  (after-init . envrc-global-mode))

;; EditorConfig.
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Useful whitespace cleanup.
(use-package ws-butler
  :hook
  (prog-mode . ws-butler-mode))

;; Parenthesis visualization.
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(provide 'init-editing)
;;; init-editing.el ends here
