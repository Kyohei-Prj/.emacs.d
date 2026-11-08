;;; init-navigation.el --- Navigation -*- lexical-binding: t; -*-

(windmove-default-keybindings) ; Shift+Arrow to switch windows

(use-package consult
  :bind
  (("C-x b" . consult-buffer)
   ("C-x C-r" . consult-recent-file)
   ("M-y" . consult-yank-pop)
   ("M-s l" . consult-line)
   ("M-s g" . consult-ripgrep)
   ("M-s L" . consult-line-multi)
   ("C-c h" . consult-history)
   ("C-c m" . consult-mark)
   ("C-c k" . consult-global-mark)))

;; Built-in project.el.
(use-package project
  :ensure nil
  :bind-keymap
  ("C-c p" . project-prefix-map))

;; Useful project shortcuts.
(with-eval-after-load 'project
  (define-key project-prefix-map
              (kbd "f") #'project-find-file)
  (define-key project-prefix-map
              (kbd "g") #'consult-ripgrep)
  (define-key project-prefix-map
              (kbd "b") #'project-switch-to-buffer)
  (define-key project-prefix-map
              (kbd "p") #'project-switch-project))

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper-backward)))

(provide 'init-navigation)
;;; init-navigation.el ends here
