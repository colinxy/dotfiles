#!/bin/bash

set -e

dotfiles_root=$(git rev-parse --show-toplevel)
elpa="$dotfiles_root/.emacs.d/elpa"
remote_elpa="$dotfiles_root/.emacs.d.remote/elpa"

packages=(
    # common dependency
    dash
    s
    f
    # use-package
    use-package
    bind-key
    diminish
    # company
    company
    # dired
    dired-hacks-utils
    dired-narrow
    dired-subtree
    # flx ido
    flx
    flx-ido
    # undo-tree
    undo-tree
    # imenu popup
    popup
    popup-imenu
    # theme
    ample-theme
)

[ -d "$elpa" ] || { echo "$elpa does not exist"; exit 1; }
[ -d "$remote_elpa" ] || mkdir -p "$remote_elpa"

for p in "${packages[@]}"; do
    echo "$elpa/$p-"[0-9]*
    cp -R "$elpa/$p-"[0-9]* "$remote_elpa"
done
