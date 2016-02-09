
set -o emacs

# export PS1="\u:\w \$ "
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced
# export PS1='\[\033[01;32m\]\u:\[\033[01;34m\]\w\[\033[00m\]\n\$ '

alias mv='mv -n'
alias cp='cp -n'
alias ll='ls -alF'
alias grep='grep --color=auto'

# emacs
alias emacs='emacs -nw'
function em {
    `which emacs` "$@" &
}
alias ect='emacsclient -t'

#function ec {
#    emacsclient -c "$@" &
#}
#export -f ec

CXXFLAGS='-std=c++11 -Wall -Wextra -Wno-sign-compare -Werror=return-type \
    -fsanitize=address -fsanitize=undefined -fno-omit-frame-pointer'

export CXXFLAGS

# powerline for bash
powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
. $powerline_root/powerline/bindings/bash/powerline.sh
