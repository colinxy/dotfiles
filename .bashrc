
# If not running interactively, don't do anything
# case $- in
#     *i*) ;;
#       *) return;;
# esac

if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi


set -o emacs

# allow parallel history
shopt -s histappend
shopt -s checkwinsize
export HISTSIZE=500000
export HISTFILESIZE=5000000
# don't put duplicate lines or lines starting with space in the history.
export HISTCONTROL=ignoreboth:erasedups

# command prompt
export PS1='\u:\w \$ '

# ls color flag
if [[ "$OSTYPE" == "darwin"* ]]; then
    export LSCOLORS=GxFxCxDxBxegedabagaced
    alias ls='ls -G'
else
    [ -x "$(type -P dircolors)" ] && {
        {
            [ -r ~/.dircolors ] && eval "$(dircolors -b ~/.dircolors)"
        } || eval "$(dircolors -b)"
    }
    alias ls='ls --color=auto'
fi
export LESS='-R'

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
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias more='more -R'
alias le=less
alias les=less
alias lesss=less
alias tree='tree -C'
export DATE_FMT='%Y-%m-%d_%H%M%S' # date +$DATE_FMT
export ISO_FMT='%Y-%m-%dT%H:%M:%SZ'

alias ulimit='ulimit -S'
sudok() { sudo "$@"; sudo -K; } # sudo -K clears cached credentials

# programmable completion
bash_completion=/etc/bash_completion
if [[ "$OSTYPE" == "darwin"* ]]; then
    bash_completion="$(brew --prefix)${bash_completion}"
fi
# [ -f "$bash_completion" ] || bash_completion=/etc/profile.d/bash_completion.sh
# shellcheck source=/dev/null
# [ -f "$bash_completion" ] && . "$bash_completion"
# unset bash_completion

# tmux
alias tmux-copy='tmux load-buffer -'   # loadb ($CMD | tmux-copy)
alias tmux-paste='tmux save-buffer -'  # saveb (tmux-paste | $CMD)
tmux-copy-from() {
    local copy=
    if type -P xclip &>/dev/null; then
        copy='xclip -selection clipboard'
    elif type -P pbcopy &>/dev/null; then
        copy='pbcopy'
    else
        >&2 echo 'xclip/pbcopy does not exist'
        return 1
    fi
    ( tmux save-buffer - | $copy )
    # text copied from tmux now exists in system clipboard
    # for paste, use `xclip -o -selection clipboard' or `pbpaste'
}
tmux-ssh-env() {
    eval "$(tmux show-env | grep '^SSH_')"
}

# fix ssh and detaching/reattaching tmux
if [[ -S "$SSH_AUTH_SOCK" && ! -h "$SSH_AUTH_SOCK" ]]; then
    ln -sf "$SSH_AUTH_SOCK" ~/.ssh/ssh_auth_sock;
fi
export SSH_AUTH_SOCK=~/.ssh/ssh_auth_sock;

# vim color hightlighter as less
vless_setup() {
    local vers=(82 81 80 74 73)
    for ver in "${vers[@]}"; do
        if [ -f "/usr/share/vim/vim${ver}/macros/less.vim" ]; then
            # shellcheck disable=SC2139
            # expansion of ${ver} at define time is intended
            alias vless="vim -u /usr/share/vim/vim${ver}/macros/less.vim -"
            break
        fi
    done
}
vless_setup
unset -f vless_setup

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
export ALTERNATE_EDITOR=''

ect() {
    emacsclient -q -t "$@"   # &>/dev/null
}
alias kill-em-daemon='emacsclient -e "(save-buffers-kill-emacs)"'
# ec() { emacsclient -c "$@" & }

# file url
fileurl() {
    # TODO: support space in filename
    echo "file://$PWD/$1"
}

# git version control
# alias git-push='git pull --rebase && git push origin -u'
alias git-log='git log --oneline --decorate --graph --all'

# diff
alias diff='diff -u'
# useful diff switches
# -E  --ignore-tab-expansion
# -Z  --ignore-trailing-space
# -b  --ignore-space-change
# -w  --ignore-all-space
# show only changes in the old/new file
alias diff-left='\diff --unchanged-line-format= --old-line-format="%L" --new-line-format='
alias diff-right='\diff --unchanged-line-format= --old-line-format= --new-line-format="%L"'

# C++
export _CXXFLAGS='-std=c++11 -Wall -Wextra -Wno-sign-compare
       -Werror=return-type -fstrict-overflow -Wstrict-overflow
       -fsanitize=address -fsanitize=undefined -fsanitize=bounds
       -fno-omit-frame-pointer'
# for 2's complement arithmetic, use -fwrapv

# Golang
alias go-run='go run !(*_test).go'

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
# useful curl switches
# curl -svo /dev/null <url>
# -s : silent, useful when piping output
# -i : include header in reponse
# -D - : dump header to stdout
# -o : output to file
# -I : HEAD
# -L : follow redirect
# -v : verbose
# -w : -w "%{http_code} %{content_type} %{size_download}\n"
#         "%{time_starttransfer}" (time to first byte)
#          %{url_effective} (use with -L)
# -d : POST, Content-Type: application/x-www-form-urlencoded
#      -d name=daniel -d skill=lousy
# -F : POST form submission, Content-Type: multipart/form-data
#      @ file upload, < contents for text field from file
#      -F name=John -F profile=@portrait.png -F "story=<hugefile.txt"
# --trace-ascii - : dump all incoming and outgoing data to stdout

# ${var//Pattern/Replacement} Global replacement
urlencode() {
    local LC_ALL=C
    for ((i=0; i<${#1}; i++)); do
        : "${1:i:1}"
        case "$_" in
            [a-zA-Z0-9.~_-])
                printf '%s' "$_";;
            *)
                printf '%%%02X' "'$_";;
        esac
    done
    printf '\n'
}
urldecode() { : "${*//+/ }"; printf '%b\n' "${_//%/\\x}"; }

# networking
alias traceroute='traceroute -n' # don't do reverse lookup
# alias dig='dig +noall +answer'  # DNS
# understand dns lookup process: +trace
# whois
# use whois to lookup ip to get more accurate results, example:
# whois $(dig google.com +short | head -1)
whois() { "$(type -P whois)" "$@" | grep -vE '^(#|\s*$)'; }
# GUI wirshark
alias ws='startprocess wireshark'
# check tcp connection with bash
# http://stackoverflow.com/questions/9609130/quick-way-to-find-if-a-port-is-open-on-linux
tcpconn() {
    [ -z "$1" ] && echo "tcpconn <ip> <port>" && return 1
    local ip='127.0.0.1'
    local port=80
    if [[ "$1" =~ $IPv4_E ]]; then
        ip=$1
        [ ! -z "$2" ] && port=$2
    elif [[ "$1" =~ ^[0-9]{1,6}$ ]]; then
        port=$1
    else
        echo "tcpconn <ip> <port>" && return 1
    fi
    echo "exec 6<>/dev/tcp/$ip/$port"
    exec 6<>"/dev/tcp/$ip/$port" &&
        echo "$ip:$port listening" || echo "$ip:$port NOT listening"
    # send http request
    # echo -ne "GET / HTTP/1.0\r\n" >&6 && cat <&6
    exec 6>&- # close output connection
    exec 6<&- # close input connection
}
# ssl certificates
# https://serverfault.com/questions/661978/displaying-a-remote-ssl-certificate-details-using-cli-tools
showcert() {
    local host="$1"
    local port="${2:-443}"
    : | openssl s_client -showcerts -servername "$host" -connect "$host":"$port" 2>/dev/null |\
        openssl x509 -inform pem -noout -text
}
# nmap
# -Pn : no ping
# -sS : TCP SYN scan
# -sT : TCP connect scan
# tcpdump
# when testing localhost, use specify loopback interface with "-i lo"
alias tcpdump_mac='sudo tcpdump -n -vv -tttt -i lo0'
# -i lo : loopback interface
# -n    : no dns resolution
# -tttt : human readable time stamp
# -vv   : verbose
# -X    : show contents
# filters : host, port, portrange, dst/src, net
# tcp flags: S SYC
#            . ACK
#            F FIN
#            R RST
#            P PUSH

# disk usage
alias du='du -hs'
alias df='df -h'

# virtualbox
alias vbox='VBoxManage list runningvms'
vstart() { VBoxManage startvm "$1" --type headless; }
vshutdown() { VBoxManage controlvm "$1" acpipowerbutton; }

# vargrant completion offered through contrib/bash/completion.sh
# complete -W "$(vagrant --help | awk '/^[[:space:]]/ {print $1}')" vagrant

export PYTHONSTARTUP="$HOME/.pythonrc"
# pip3upgradeall() {
#     python3 -m pip list --outdated | tail -n +3 | awk '{print $1}' | xargs python3 -m pip install -U # --user
# }
# pipupgradeall() {
#     python -m pip list --outdated | tail -n +3 | awk '{print $1}' | xargs python -m pip install -U # --user
# }

# personal accounts
# shellcheck source=/dev/null
[ -f "$HOME"/.accounts ] && . "$HOME"/.accounts

# shellcheck source=/dev/null
[ -f "$HOME"/.bash_colors ] && . "$HOME"/.bash_colors

settitle() {
    echo -e '\033];'"$*"'\007'
}

pathmunge () {
    case ":${PATH}:" in
        *:"$1":*)
            ;;
        *)
            if [ "$2" = "after" ] ; then
                PATH=$PATH:$1
            else
                PATH=$1:$PATH
            fi
    esac
}

# fun
# history | awk '{a[$2]++} END {for(i in a){print a[i] " " i}}' | sort -rn | head
# nc -v -l 8080 < afile
# fortune | cowsay -f $(find /usr/share/cowsay/ -name '*.cow' | shuf -n1)
# telnet telehack.com
