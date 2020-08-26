(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:output-area-inlined-images t)
 '(package-selected-packages
   (quote
    (ein multi-term helm elpy yasnippet s pyvenv highlight-indentation elscreen company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'multi-term)
(setq multi-term-program "/bin/bash")

(require 'elscreen)
(elscreen-start)

(setq inhibit-startup-message t)
(global-linum-mode t)

(elpy-enable)

(windmove-default-keybindings)
(global-set-key (kbd "C-j") 'goto-line)
(global-set-key (kbd "C-h") 'helm-for-files)
