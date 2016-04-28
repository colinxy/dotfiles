
# backup
[ -f ~/.bashrc ] && mv ~/.bashrc ~/.bashrc.bak
[ -f ~/.emacs ] && mv ~/.emacs ~/.emacs.bak
[ -f ~/.vimrc ] && mv ~/.vimrc ~/.vimrc.bak

# create symbolic link
ln -s "$PWD"/.bashrc ~/.bashrc
ln -s "$PWD"/.emacs ~/.emacs
ln -s "$PWD"/.vimrc ~/.vimrc
