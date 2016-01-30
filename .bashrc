
set -o emacs

# Setting PATH for Python 3.4
export PATH="/Library/Frameworks/Python.framework/Versions/3.4/bin:$PATH"

# export PS1="\u:\w \$ "
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced
# export PS1='\[\033[01;32m\]\u:\[\033[01;34m\]\w\[\033[00m\]\n\$ '

alias ll='ls -alF'
alias grep='grep --color=auto'

# emacs
alias emacs='emacs -nw'
alias ect='emacsclient -t'

#function ec {
#    emacsclient -c "$@" &
#}

#export -f ec

# dealing with seasnet server
function seas {
    echo "xinyuy@lnxsrv$1.seas.ucla.edu"
}

export -f seas

CXXFLAGS='-std=c++11 -Wall -Wextra -Wno-sign-compare -Werror=return-type \
    -fsanitize=address -fsanitize=undefined -fno-omit-frame-pointer'

export CXXFLAGS

# powerline for bash
powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
. /usr/local/lib/python2.7/site-packages/powerline/bindings/bash/powerline.sh
