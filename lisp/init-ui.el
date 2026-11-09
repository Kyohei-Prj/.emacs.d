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

;; Modeline.
(setq mode-line-format
      '("%e"
        mode-line-front-space
        mode-line-buffer-identification
        "  "
        mode-line-position
        "  "
        mode-line-modes
        mode-line-end-spaces))

;; Line numbers for programming.
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode 1)))

;; Avoid visual clutter in terminal.
(setq visible-bell nil
      cursor-in-non-selected-windows nil
      blink-cursor-mode nil)

;; Do not use GUI-only fringe.
(setq indicate-buffer-boundaries nil)

;; keybinding helper
(use-package which-key
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0.05))

(provide 'init-ui)
;;; init-ui.el ends here
