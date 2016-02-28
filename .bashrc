
set -o emacs

# export PS1="\u:\w \$ "
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced
# export PS1='\[\033[01;32m\]\u:\[\033[01;34m\]\w\[\033[00m\]\n\$ '

alias mv='mv -i'
alias cp='cp -i'
alias la='ls -a'
alias ll='ls -alF'
alias grep='grep --color'
alias more='more -R'
alias less='less -R'

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
