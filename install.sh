#!/bin/bash

# vim Vundle
git clone \
    https://github.com/VundleVim/Vundle.vim.git \
    ~/.vim/bundle/Vundle.vim

vim +PluginInstall +qall


# emacs
sed -i '1s/^;; \(setq use-package-always-ensure t\)/(setq use-package-always-ensure t)/' "$HOME"/.emacs.d/init.el

emacs --batch -l "$HOME"/.emacs.d/init.el

sed -i '1s/^\(setq use-package-always-ensure t\)/;; (setq use-package-always-ensure t)/' "$HOME"/.emacs.d/init.el
