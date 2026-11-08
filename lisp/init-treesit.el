;;; init-treesit.el --- Tree-sitter -*- lexical-binding: t; -*-

(require 'treesit)

(setq treesit-language-source-alist
      '((bash
         "https://github.com/tree-sitter/tree-sitter-bash")

        (css
         "https://github.com/tree-sitter/tree-sitter-css")

        (html
         "https://github.com/tree-sitter/tree-sitter-html")

        (javascript
         "https://github.com/tree-sitter/tree-sitter-javascript")

        (json
         "https://github.com/tree-sitter/tree-sitter-json")

        (python
         "https://github.com/tree-sitter/tree-sitter-python")

        (tsx
         "https://github.com/tree-sitter/tree-sitter-typescript"
         "master" "tsx/src")

        (typescript
         "https://github.com/tree-sitter/tree-sitter-typescript"
         "master" "typescript/src")

        (yaml
         "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

;; Mode associations.
(add-to-list 'auto-mode-alist
             '("\\.ts\\'" . typescript-ts-mode))

(add-to-list 'auto-mode-alist
             '("\\.tsx\\'" . tsx-ts-mode))

(add-to-list 'auto-mode-alist
             '("\\.jsx\\'" . js-ts-mode))

(add-to-list 'auto-mode-alist
             '("\\.js\\'" . js-ts-mode))

(add-to-list 'auto-mode-alist
             '("\\.mjs\\'" . js-ts-mode))

(add-to-list 'auto-mode-alist
             '("\\.cjs\\'" . js-ts-mode))

(add-to-list 'auto-mode-alist
             '("\\.py\\'" . python-ts-mode))

(add-to-list 'auto-mode-alist
             '("\\.json\\'" . json-ts-mode))

(add-to-list 'auto-mode-alist
             '("\\.ya?ml\\'" . yaml-ts-mode))

(add-to-list 'auto-mode-alist
             '("\\.sh\\'" . bash-ts-mode))

;; zsh is not Bash.
(add-to-list 'auto-mode-alist
             '("\\.zsh\\'" . sh-mode))

;; Indentation.
(setq-default python-indent-offset 4
              js-indent-level 2
              css-indent-offset 2
              typescript-ts-indent-offset 2)

(provide 'init-treesit)
;;; init-treesit.el ends here
