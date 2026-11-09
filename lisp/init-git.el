;;; init-git.el --- Git integration -*- lexical-binding: t; -*-

(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-c g l" . magit-log-current)
   ("C-c g b" . magit-blame))
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-display-buffer-function
   #'magit-display-buffer-fullframe-status-v1))

(provide 'init-git)
;;; init-git.el ends here
