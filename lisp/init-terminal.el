;;; init-terminal.el --- Terminal integration -*- lexical-binding: t; -*-

;; Custom tab-bar-mode (C-z)
(use-package tab-bar
  :ensure nil
  :init (tab-bar-mode 1)
  :config (setq tab-bar-show 1
                tab-bar-close-button-show nil
                tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  ;; Custom Tab Name Formatting (0-indexed)
  (setq tab-bar-tab-name-format-function
        (lambda (tab i)
          (let ((face (funcall tab-bar-tab-face-function tab)))
            (propertize
             (format " %d: %s " (- i 1) (alist-get 'name tab))
             'face face))))
  ;; Keymaps
  (define-prefix-command 'my-tab-bar-map)
  (global-set-key (kbd "C-z") 'my-tab-bar-map)
  (define-key my-tab-bar-map (kbd "c") 'tab-bar-new-tab)
  (define-key my-tab-bar-map (kbd "k") 'tab-bar-close-tab)
  ;; Define keys 0-9
  (dotimes (i 10)
    (define-key my-tab-bar-map (kbd (number-to-string i))
                `(lambda () (interactive) (tab-bar-select-tab ,(+ i 1))))))

;; terminal emulation
(use-package vterm
  :custom
  ;; vterm forwards every key to the shell unless listed here, so `C-z' has
  ;; to be an exception or the tab prefix below would send SIGTSTP instead.
  ;; Keep the upstream defaults in sync (C-h v vterm-keymap-exceptions).
  (vterm-keymap-exceptions
   '("C-c" "C-x" "C-u" "C-g" "C-h" "C-l" "M-x" "M-o" "C-y" "M-y" "C-z"))
  :bind (:map vterm-mode-map
              ("C-c ESC" . vterm-send-escape)))

(provide 'init-terminal)
;;; init-terminal.el ends here
