
set -o emacs


# command prompt
# export PS1="\u:\w \$ "
export PS1='\[\033[01;32m\]\u:\[\033[01;34m\]\w\[\033[00m\]\n\$ '

if [[ "$OSTYPE" == "darwin"* ]]; then
    export LSCOLORS=GxFxCxDxBxegedabagaced
    alias ls='ls -G'
else
    alias ls='ls --color=auto'
fi

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias mv='mv -i'
alias cp='cp -i'
alias la='ls -a'
alias ll='ls -alF'
alias grep='grep --color'
alias more='more -R'
alias less='less -R'
alias tree='tree -C'

# vim color hightlighter as less
if [ -f /usr/share/vim/vim74/macros/less.vim ]; then
    alias vless='vim -u /usr/share/vim/vim74/macros/less.vim -'
elif [ -f /usr/share/vim/vim73/macros/less.vim ]; then
    alias vless='vim -u /usr/share/vim/vim73/macros/less.vim -'
fi

# emacs
if [[ "$OSTYPE" == "darwin"* ]]; then
    alias em='open -a emacs --args --chdir $PWD'
else
    function em { $(which emacs) "$@" & }
fi

alias emacs='emacs -nw'

export EDITOR='emacsclient -t'
export ALTERNATE_EDITOR=''

# function ect {
#     emacsclient -q -t "$@" # &>/dev/null
# }

# function ec {
#    emacsclient -c "$@" &
# }

# git version control
alias git-lazy='git add . && git commit -m'
alias git-master='git push origin master'
alias git-visual='git log --graph --decorate --oneline'

# C++
CXXFLAGS='-std=c++11 -Wall -Wextra -Wno-sign-compare -Werror=return-type -fsanitize=address -fsanitize=undefined -fno-omit-frame-pointer'

export CXXFLAGS

# linux utilities on Mac OSX
if [[ "$OSTYPE" == "darwin"* ]]; then
    alias ldd='otool -L'
    alias objdump='otool -tV'
fi

# LISP
# readline wrapper
alias scheme='rlwrap scheme'
alias sbcl-repl='rlwrap sbcl'

function clisp-run {
    clisp -q -c "$1"
    time clisp -q -on-error abort -x "(progn (load \"${1%%.*}\") (quit))"
}
