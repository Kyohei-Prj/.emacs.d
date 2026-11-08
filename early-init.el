;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

(setq frame-inhibit-implied-resize t
      inhibit-startup-screen t
      inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Reduce garbage collection during startup.
(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024)
                  gc-cons-percentage 0.1)))

;;; early-init.el ends here
