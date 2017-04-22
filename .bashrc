
set -o emacs

# allow parallel history
shopt -s histappend

# command prompt
export PS1="\u:\w \$ "

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

startprocess() {
    nohup "$@" >/dev/null 2>&1 &
    disown %+
}

# emacs
alias em='startprocess "$(type -P emacs)"'
alias edit='startprocess "$(type -P emacs)" -q --load ~/.emacs.min'

alias emacs='emacs -nw'
# export EDITOR='emacsclient -t'
export EDITOR='vim'
# export ALTERNATE_EDITOR=''

ect() {
    emacsclient -q -t "$@"   # &>/dev/null
}
alias kill-em-daemon='emacsclient -e "(save-buffers-kill-emacs)"'
# ec() { emacsclient -c "$@" & }

# git version control
alias git-push='git pull --rebase && git push origin -u'
alias git-log='git log --oneline --decorate --graph --all'

# diff
alias diff='diff -u'
# useful diff switches
# -E  --ignore-tab-expansion
# -Z  --ignore-trailing-space
# -b  --ignore-space-change
# -w  --ignore-all-space

# C++
export CXXFLAGS='-std=c++11 -Wall -Wextra -Wno-sign-compare
       -Werror=return-type -fstrict-overflow -Wstrict-overflow
       -fsanitize=address -fsanitize=undefined -fsanitize=bounds
       -fno-omit-frame-pointer'
# for 2's complement arithmetic, use -fwrapv

# LISP
# repl readline wrapper
alias scheme='rlwrap scheme'
alias sbcl-repl='rlwrap sbcl'

clisp-run() {
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
# alias serve='python -m SimpleHTTPServer'
alias serve='python3 -m http.server --bind 127.0.0.1'
export IPv4='[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}'
# for use with extended regex (grep -E)
export IPv4_E='[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}'

# network
# alias dig='dig +noall +answer'  # DNS
# whois
# use whois to lookup ip to get more accurate results, example:
# whois $(dig google.com | head -1 | awk '{print $5}')
whois() { "$(type -P whois)" "$@" | grep -vE '^(#|\s*$)'; }
# GUI wirshark
alias ws='startprocess wireshark'
# check tcp connection with bash
# http://stackoverflow.com/questions/9609130/quick-way-to-find-if-a-port-is-open-on-linux
tcpconn() {
    [ -z "$1" ] && echo "tcpconn <ip> <port>" && return 1
    local ip='127.0.0.1'
    local port=80

    if [[ "$1" =~ $IPV4_E ]]; then
        ip=$1
        [ ! -z "$2" ] && port=$2
    elif [[ "$1" =~ ^[0-9]{1,6}$ ]]; then
        port=$1
    else
        echo "tcpconn <ip> <port>" && return 1
    fi

    echo "exec 6<>/dev/tcp/$ip/$port"
    exec 6<>"/dev/tcp/$ip/$port" &&
        echo "$port listening" || echo "$port not listening"
    # send http request
    # echo -e "GET / HTTP/1.0\n" >&6 && cat <&6
    exec 6>&- # close output connection
    exec 6<&- # close input connection
}

# disk usage
alias du='du -hs'
alias df='df -h'

# virtualbox
alias vbox='VBoxManage list runningvms'
alias ubuntu='VBoxManage startvm "ubuntu16" --type headless'
alias centos='VBoxManage startvm "centos7" --type headless'
alias freebsd='VBoxManage startvm "freebsd11" --type headless'
vshutdown() { VBoxManage controlvm "$1" acpipowerbutton; }

# vargrant completion offered through contrib/bash/completion.sh
# complete -W "$(vagrant --help | awk '/^[[:space:]]/ {print $1}')" vagrant

# python virtual environment
[ -r /usr/local/opt/autoenv/activate.sh ] && . /usr/local/opt/autoenv/activate.sh

# personal accounts
[ -f "$HOME"/.accounts ] && . "$HOME"/.accounts

set-title() {
    echo -e "\033];$*\007"
}

# fun
# history | awk '{a[$2]++} END {for(i in a){print a[i] " " i}}' | sort -rn | head
# nc -v -l 8080 < afile
# fortune | cowsay -f $(ls /usr/share/cowsay/cows/ | shuf -n1)
# nc -v -l 8080 < afile
# telnet telehack.com
