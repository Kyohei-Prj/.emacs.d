;;; init-terminal.el --- Terminal integration -*- lexical-binding: t; -*-

(use-package eat
  :ensure t
  :bind
  (("C-c t" . my/project-terminal)
   ("C-c T" . eat)))

(defun my/project-terminal ()
  "Open Eat at the current project root."
  (interactive)
  (let ((default-directory
         (if-let* ((project (project-current nil)))
             (project-root project)
           default-directory)))
    (eat)))

(provide 'init-terminal)
;;; init-terminal.el ends here
