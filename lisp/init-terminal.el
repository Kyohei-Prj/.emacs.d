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
  ;; `my/vterm-new-tab' is defined further down with `multi-vterm'; binding a
  ;; symbol keeps the terminal package lazy (it loads when the key is used).
  (define-key my-tab-bar-map (kbd "t") #'my/vterm-new-tab)
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

;; Multiple terminals: the multi-term.el workflow on top of the `vterm' buffers
;; above.  `multi-vterm' keeps its own list of terminal buffers so `next'/`prev'
;; cycle through them, adds a project-scoped terminal and a dedicated bottom
;; window; every buffer is still a plain `vterm-mode' one, so
;; `vterm-keymap-exceptions' and `C-c ESC' keep applying to all of them.
;;
;; The commands live on the `C-c t' prefix.  Inside a terminal, `vterm-mode-map'
;; only defines `C-c' as a prefix (`C-c C-c', `C-c C-l', `C-c C-y'), and a
;; sequence a higher-priority prefix cannot complete falls through to the global
;; map, so `C-c t <key>' reaches these commands from the shell too.
(use-package multi-vterm
  :custom
  ;; Buffers are then *vterm<1>*, *vterm<2>*, ... and *vterm - <project>*.
  (multi-vterm-buffer-name "vterm")
  ;; Wins over `multi-vterm-dedicated-window-height' when non-nil; upstream
  ;; clamps the value to 10-90% of the window's rows.
  (multi-vterm-dedicated-window-height-percent 30)
  :bind
  (("C-c t t" . multi-vterm)
   ("C-c t n" . multi-vterm-next)
   ("C-c t p" . multi-vterm-prev)
   ("C-c t P" . multi-vterm-project)
   ("C-c t r" . multi-vterm-rename-buffer)
   ("C-c t d" . multi-vterm-dedicated-toggle)))

;; `multi-vterm' replaces the selected window, while this config keeps one
;; context per tab: `C-z t' gives a terminal a whole tab instead.  Naming the
;; tab is part of the job, otherwise `tab-bar' labels it with the `*vterm<N>*'
;; buffer name.
(defun my/vterm-new-tab ()
  "Open a `multi-vterm' terminal that fills a new tab.
Like `tab-new', the tab clones the window configuration of the tab being
left, so the terminal starts in that `default-directory' (usually the
project); the other windows are then deleted and the tab is named after that
project or directory, because `tab-bar' would otherwise label the tab with
the `*vterm<N>*' buffer name."
  (interactive)
  (let ((tabs-before (length (funcall tab-bar-tabs-function))))
    (tab-new)
    ;; `tab-new' returns the result of the last tab-bar redraw, not a success
    ;; flag, and only messages when it refuses to add a tab, so compare the
    ;; tab list instead.
    (when (= tabs-before (length (funcall tab-bar-tabs-function)))
      (user-error "Could not create a new tab")))
  (delete-other-windows)
  ;; Resolve the name before `multi-vterm' switches buffers.
  (let ((name (or (and-let* ((proj (project-current))) (project-name proj))
                  (file-name-nondirectory
                   (directory-file-name default-directory)))))
    (multi-vterm)
    ;; `tab-bar-rename-tab' treats an empty NAME as "use the automatic name",
    ;; which is what a tab rooted at "/" would otherwise produce.
    (tab-bar-rename-tab (if (string-empty-p name) default-directory name))))

(provide 'init-terminal)
;;; init-terminal.el ends here
