;;; init-core.el --- Core settings -*- lexical-binding: t; -*-

;; Compile-time only: silences "free variable" warnings for the shell
;; variables set below; nothing is loaded at runtime.
(eval-when-compile (require 'shell))

;; `shell' resolves `explicit-SHELL-args' by name at runtime, so the zsh one
;; has no definition to pick up (same as `explicit-bash-args').
(defvar explicit-zsh-args)

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

;; Backup files go to ~/.emacs.d/backups as numbered versions, so saving
;; repeatedly never destroys the previous revision.
(setq backup-directory-alist `(("." . ,(locate-user-emacs-file "backups/")))
      backup-by-copying t
      vc-make-backup-files t
      delete-old-versions t)

;; Auto-save files (#foo#) belong in the user directory too, not next to the
;; file being edited.  Names are flattened, so files with the same basename
;; in different directories share one auto-save file.
(make-directory (locate-user-emacs-file "auto-save/") t)
(setq auto-save-file-name-transforms
      `((".*" ,(locate-user-emacs-file "auto-save/") t)))

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

;; Show trailing whitespace in code buffers; `ws-butler' only cleans the
;; lines that are actually edited, so the rest needs to stay visible.
;; `show-trailing-whitespace' is buffer-local, hence `setq-local'.
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local show-trailing-whitespace t)))

;; Make the toolchain available to subprocesses (language servers, ripgrep,
;; ...).  `my/add-to-path' prepends, so list the lowest-priority directory
;; first: personal tool directories must win over the system ones, otherwise
;; a stale /usr/local/bin copy shadows a `uv tool install' build.  GUI Emacs
;; on macOS does not inherit the shell PATH, hence /opt/homebrew/bin.
(defun my/add-to-path (directory)
  "Prepend DIRECTORY to `exec-path' and `$PATH' if it exists.
Existing copies of DIRECTORY are dropped from `$PATH' first, so neither
a repeated startup nor an inherited shell PATH can grow it."
  (let ((dir (expand-file-name directory)))
    (when (file-directory-p dir)
      (add-to-list 'exec-path dir)
      (setenv "PATH"
              (mapconcat #'identity
                         (cons dir
                               (delete dir
                                       (split-string (or (getenv "PATH") "")
                                                     path-separator t)))
                         path-separator)))))

(dolist (directory '("/usr/local/bin"
                     "/opt/homebrew/bin"
                     "~/.cargo/bin"
                     "~/.local/bin"
                     "~/.bun/bin"))
  (my/add-to-path directory))

;; Prefer zsh for shell commands when it is installed; hardcoding /bin/zsh
;; breaks on hosts where it lives elsewhere (or nowhere).
(when-let* ((zsh (executable-find "zsh")))
  (setq shell-file-name zsh
        explicit-shell-file-name zsh
        explicit-zsh-args '("-l")))

;; Use short answers.
(setq use-short-answers t)

;; Keep the minibuffer history (and therefore the Vertico/Consult history)
;; across sessions; it is saved to `~/.emacs.d/history', which is gitignored.
(savehist-mode 1)

(setq enable-recursive-minibuffers t
      ;; TAB indents, and completes when the line is already indented.
      tab-always-indent 'complete
      ;; M-x hides commands that cannot apply in the current context.
      read-extended-command-predicate
      #'command-completion-default-include-p)

(provide 'init-core)
;;; init-core.el ends here
