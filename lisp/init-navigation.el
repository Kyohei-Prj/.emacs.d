;;; init-navigation.el --- Navigation -*- lexical-binding: t; -*-

;; Compile-time only: `xref-show-*-function' are set below and are defined in
;; xref.el; nothing is loaded at runtime.
(eval-when-compile (require 'xref))

(windmove-default-keybindings) ; Shift+Arrow to switch windows

;; Buffer search stays on the built-in incremental isearch (C-s / C-r): it is
;; symmetric, works in a terminal, and integrates with Embark.  Consult's
;; non-incremental line search is on M-s l / M-s L.  Swiper was dropped
;; because it drags Ivy in and does not use Vertico, Orderless or Embark.
;; For Consult on C-s instead, add: (global-set-key (kbd "C-s") #'consult-line)
(use-package consult
  :functions (consult-xref)   ;; defined in consult-xref.el
  :bind
  (("C-x b" . consult-buffer)
   ("C-x C-r" . consult-recent-file)
   ("M-y" . consult-yank-pop)
   ("M-s l" . consult-line)
   ("M-s g" . consult-ripgrep)
   ("M-s L" . consult-line-multi)
   ("C-c h" . consult-history)
   ("C-c m" . consult-mark)
   ("C-c k" . consult-global-mark)
   ("M-g f" . consult-flymake)
   ([remap goto-line] . consult-goto-line)
   ([remap imenu] . consult-imenu))
  :init
  ;; Select xref locations with Consult, with previews and narrowing.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; Built-in project.el.
(use-package project
  :ensure nil
  :bind-keymap
  ("C-c p" . project-prefix-map))

;; `f' (find-file), `b' (switch-to-buffer) and `p' (switch-project) are
;; already in `project-prefix-map'; only replace the default `project-search'
;; on `g' with ripgrep.
(with-eval-after-load 'project
  (define-key project-prefix-map (kbd "g") #'consult-ripgrep))

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(provide 'init-navigation)
;;; init-navigation.el ends here
