;;; init-completion.el --- Completion -*- lexical-binding: t; -*-

;; Vertico: minibuffer completion.
(use-package vertico
  :init
  (vertico-mode 1)
  :custom
  (vertico-cycle t)
  (vertico-resize t))

;; Orderless: flexible matching.
;; With `completion-category-defaults' nil every category that has no
;; override -- including `eglot' and `eglot-capf' -- falls back to
;; `completion-styles', i.e. (orderless basic), so the category overrides the
;; Orderless README suggests for Eglot are not needed here.
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

;; Corfu: in-buffer completion.  `corfu-auto.el' is loaded by Corfu itself
;; when `corfu-auto' is non-nil, so it does not need requiring here.
(use-package corfu
  :bind
  ;; M-i completes; TAB already does, via `tab-always-indent'.
  (("M-i" . completion-at-point))
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-preselect 'first)
  (corfu-preview-current nil)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  :config
  (corfu-popupinfo-mode 1))

;; Additional completion sources.
;; `completion-at-point-functions' is buffer-local by default, so `add-to-list'
;; during init writes only the startup buffer's local value and the sources
;; never reach ordinary buffers -- `add-hook' edits the global default instead.
;; Hooks are prepended too, hence the reversed order below: the first Capf
;; returning a result wins, so file names beat keywords beat dabbrev, with
;; dabbrev as the last resort.  Buffer-local Capfs (Eglot, major modes) take
;; precedence over this global list anyway.
(use-package cape
  :init
  (dolist (capf (list #'cape-dabbrev #'cape-keyword #'cape-file))
    (add-hook 'completion-at-point-functions capf)))

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
