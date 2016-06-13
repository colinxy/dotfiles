
# backup and create symbolic link
[ -f ~/.bashrc ] && mv ~/.bashrc ~/.bashrc.bak && ln -s "$PWD"/.bashrc ~/.bashrc
[ -f ~/.emacs ] && mv ~/.emacs ~/.emacs.bak && ln -s "$PWD"/.emacs ~/.emacs
[ -f ~/.vimrc ] && mv ~/.vimrc ~/.vimrc.bak && ln -s "$PWD"/.vimrc ~/.vimrc
