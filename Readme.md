# lsp booster
https://github.com/blahgeek/emacs-lsp-booster/releases

# common lisp
sudo apt install sbcl

curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp

sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' --eval '(ql:add-to-init-file)' --quit

# python
mkdir -p ~/miniconda3

wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda3/miniconda.sh

bash ~/miniconda3/miniconda.sh -b -u -p ~/miniconda3

rm -rf ~/miniconda3/miniconda.sh

~/miniconda3/bin/conda init zsh

curl -sSL https://install.python-poetry.org | python3 -

mkdir $ZSH_CUSTOM/plugins/poetry

poetry completions zsh > $ZSH_CUSTOM/plugins/poetry/_poetry

poetry add pyright tree-sitter-languages mypy autopep8 pytest jupyter --group dev

# rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# c/c++
sudo apt install clang cmake
