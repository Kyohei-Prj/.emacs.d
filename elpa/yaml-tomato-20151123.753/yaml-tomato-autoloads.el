;;; yaml-tomato-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "yaml-tomato" "yaml-tomato.el" (0 0 0 0))
;;; Generated autoloads from yaml-tomato.el

(autoload 'yaml-tomato-show-current-path "yaml-tomato" "\
Show current yaml path in message buffer.

\(fn)" t nil)

(autoload 'yaml-tomato-copy "yaml-tomato" "\
Copy current path to 'kill-ring'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yaml-tomato" '("yaml-tomato--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; yaml-tomato-autoloads.el ends here
