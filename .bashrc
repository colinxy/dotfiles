
set -o emacs


# command prompt
# export PS1="\u:\w \$ "

# ls color flag
if [[ "$OSTYPE" == "darwin"* ]]; then
    export LSCOLORS=GxFxCxDxBxegedabagaced
    alias ls='ls -G'
else
    alias ls='ls --color=auto'
fi
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias rm='rm -iv'
alias mv='mv -i'
alias cp='cp -i'
alias la='ls -A'
alias ll='ls -alF'
alias l='ls -CF'
alias grep='grep --color=auto'
alias more='more -R'
alias less='less -R'
alias tree='tree -C'

# programmable completion
MY_BASH_COMPLETION=/etc/bash_completion
if [[ "$OSTYPE" == "darwin"* ]]; then
    MY_BASH_COMPLETION="$(brew --prefix)${MY_BASH_COMPLETION}"
fi
[ -f "$MY_BASH_COMPLETION" ] && . "$MY_BASH_COMPLETION"

# vim color hightlighter as less
if [ -f /usr/share/vim/vim74/macros/less.vim ]; then
    alias vless='vim -u /usr/share/vim/vim74/macros/less.vim -'
elif [ -f /usr/share/vim/vim73/macros/less.vim ]; then
    alias vless='vim -u /usr/share/vim/vim73/macros/less.vim -'
fi

# emacs
if [[ "$OSTYPE" == "darwin"* ]]; then
    alias em='open -a /Applications/Emacs.app --new --args --chdir $PWD'
    alias edit='open -a /Applications/Emacs.app --new --args --chdir $PWD -q --load ~/.emacs.min'
else
    function em { $(which emacs) "$@" 2>/dev/null & }
    # alias edit='\emacs -q --load ~/.emacs.min &>/dev/null &'
fi

alias emacs='emacs -nw'
# export EDITOR='emacsclient -t'
export EDITOR='vim'
# export ALTERNATE_EDITOR=''

ect() {
    emacsclient -q -t "$@"   # &>/dev/null
}
alias em-proc='pgrep -lf [eE]macs'
alias kill-em-daemon='emacsclient -e "(save-buffers-kill-emacs)"'
# function ec { emacsclient -c "$@" & }

# git version control
alias git-push='git push origin -u'
alias git-log='git log --oneline --decorate --graph --all'

# diff
alias diff='diff -u'
# useful diff switches
# -E  --ignore-tab-expansion
# -Z  --ignore-trailing-space
# -b  --ignore-space-change
# -w  --ignore-all-space

# C++
export CXXFLAGS='-std=c++11 -Wall -Wextra -Wno-sign-compare -Werror=return-type -fsanitize=address -fsanitize=undefined -fno-omit-frame-pointer'

# LISP
# repl readline wrapper
alias scheme='rlwrap scheme'
alias sbcl-repl='rlwrap sbcl'

function clisp-run {
    clisp -q -c "$1"
    time clisp -q -on-error abort -x "(progn (load \"${1%%.*}\") (quit))"
}

# linux utilities on Mac OSX
if [[ "$OSTYPE" == "darwin"* ]]; then
    alias ldd='otool -L'
    alias objdump='otool -tV'
    alias mktemp='mktemp -t tmp'
fi

# highlight
alias highlight='pygmentize -g -f terminal256 -O style=native'

# web
# equivalent: python3 -m http.server
alias serve='python -m SimpleHTTPServer'
export IP_PATTERN='[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}'

# network
alias dig='dig +noall +answer'  # DNS
# disk usage
alias du='du -hs'
alias df='df -h'

# python virtual environment
[ -r /usr/local/opt/autoenv/activate.sh ] && . /usr/local/opt/autoenv/activate.sh

# personal accounts
[ -f ~/.accounts ] && . ~/.accounts

set-title() {
    echo -e "\033];$*\007"
}

# fun
# history | awk '{a[$2]++} END {for(i in a){print a[i] " " i}}' | sort -rn | head
# nc -v -l 8080 < afile
# fortune | cowsay -f $(ls /usr/share/cowsay/cows/ | shuf -n1)
# nc -v -l 8080 < afile
# telnet telehack.com
