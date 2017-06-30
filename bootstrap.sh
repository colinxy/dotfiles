#!/bin/bash

# https://stackoverflow.com/a/246128
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

configs=(.bashrc .emacs.d .vimrc .emacs.min .tmux.conf)

for cfg in "${configs[@]}"
do
    [ -r "$HOME/$cfg" ] && mv "$HOME/$cfg" "$HOME/$cfg".bak
    ln -s "$DIR/$cfg" "$HOME/$cfg"
done
