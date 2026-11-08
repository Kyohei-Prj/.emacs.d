;;; init-completion.el --- Completion -*- lexical-binding: t; -*-

;; Vertico: minibuffer completion.
(use-package vertico
  :init
  (vertico-mode 1)
  :custom
  (vertico-cycle t)
  (vertico-resize t))

;; Orderless: flexible matching.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))

;; Marginalia: annotations.
(use-package marginalia
  :init
  (marginalia-mode 1))

;; Corfu: in-buffer completion.
(use-package corfu
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-preview-current nil)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  :config
  (corfu-popupinfo-mode 1))

;; Additional completion sources.
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions
               #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions
               #'cape-file)
  (add-to-list 'completion-at-point-functions
               #'cape-keyword))

;; Contextual completion actions.
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :after (embark consult))

(provide 'init-completion)
;;; init-completion.el ends here
