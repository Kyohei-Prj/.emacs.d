;;; init-core.el --- Core settings -*- lexical-binding: t; -*-

(setq-default
 indent-tabs-mode nil
 tab-width 2
 fill-column 88
 truncate-lines nil
 sentence-end-double-space nil)

(setq
 ring-bell-function 'ignore
 use-dialog-box nil
 make-backup-files t
 create-lockfiles nil
 auto-save-default t
 scroll-conservatively 101
 scroll-margin 5
 inhibit-startup-screen t
 require-final-newline t
 backward-delete-char-untabify-method 'all)

;; Backup files.
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 "backups/"
                 user-emacs-directory))))

;; Better default editing.
(delete-selection-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)
(global-auto-revert-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(winner-mode 1)
(repeat-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

;; Show trailing whitespace.
(setq-default show-trailing-whitespace nil)

;; Use zsh for interactive shell commands.
(setq shell-file-name "/bin/zsh"
      explicit-shell-file-name "/bin/zsh"
      explicit-zsh-args '("-l"))

;; Make Bun and uv tools available to Emacs.
(defun my/add-to-path (directory)
  "Add DIRECTORY to PATH and exec-path if it exists."
  (when (file-directory-p directory)
    (add-to-list 'exec-path directory)
    (setenv "PATH"
            (concat directory path-separator (getenv "PATH")))))

(dolist (directory '("~/.bun/bin"
                     "~/.local/bin"
                     "~/.cargo/bin"
                     "/usr/local/bin"))
  (my/add-to-path (expand-file-name directory)))

;; Use short answers.
(setq use-short-answers t)

(provide 'init-core)
;;; init-core.el ends here
