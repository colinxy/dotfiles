#!/bin/bash

set -e

# https://stackoverflow.com/a/246128
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

configs=(
    .inputrc
    .bashrc
    .bash_colors
    .pythonrc
    .emacs.min
    .vimrc.min
)

linknames=(
    .inputrc
    .bashrc
    .bash_colors
    .pythonrc
    .emacs.min
    .vimrc.min
)

for arg in "$@"
do
    case "$arg" in
        vimrc)
            configs+=(".vimrc")
            linknames+=(".vimrc");;
        vimrc.min)
            configs+=(".vimrc.min")
            linknames+=(".vimrc");;
        emacs)
            configs+=(".emacs.d")
            linknames+=(".emacs.d");;
        emacs.remote)
            configs+=(".emacs.remote.d")
            linknames+=(".emacs.d");;
        tmux)
            configs+=(".tmux.conf")
            linknames+=(".tmux.conf");;
    esac
done


for ((i=0; i<${#configs[@]}; i++))
do
    cfg="${configs[i]}"
    link="${linknames[i]}"
    [ -r "$HOME/$link" ] && mv -n "$HOME/$link" "$HOME/$link".bak
    ln -s "$DIR/$cfg" "$HOME/$link"
done
