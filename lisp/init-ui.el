;;; init-ui.el --- Terminal appearance -*- lexical-binding: t; -*-

;; Dark theme.
(require 'modus-themes)

;; `modus-themes-prompts' and `modus-themes-completions' were removed in
;; Modus Themes 5.3: prompts and completion matches are now bold as a
;; consequence of `modus-themes-bold-constructs'.
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-mixed-fonts nil
      modus-themes-org-blocks 'tinted-background)

(load-theme 'modus-vivendi t)

;; Modeline.  `mode-line-format' is buffer-local, so `setq' would only change
;; the buffer that happens to be current during startup: use `setq-default'.
;; Keep the modification marker, the VC branch and `mode-line-misc-info',
;; which is where Eglot publishes its server status.
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-modified " "
                mode-line-buffer-identification "  "
                mode-line-position
                (vc-mode vc-mode) "  "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

;; Line numbers for programming.
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode 1)))

;; Avoid visual clutter.  `cursor-in-non-selected-windows' is buffer-local
;; too, and `blink-cursor-mode' is a minor mode with a custom setter, so it
;; has to be switched off by calling the mode, not by setting its variable.
;; (`indicate-buffer-boundaries' already defaults to nil.)
(setq visible-bell nil)
(setq-default cursor-in-non-selected-windows nil)
(blink-cursor-mode -1)

;; keybinding helper
(use-package which-key
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0.05))

(provide 'init-ui)
;;; init-ui.el ends here
