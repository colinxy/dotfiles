#!/bin/bash

# source me!

# install git prompt on the fly
# reference
# https://joshtronic.com/2018/01/28/minimalist-git-prompt/

_RESET=$(tput sgr0)

_BLUE=$(tput setaf 4)
_GREY=$(tput setaf 244)
_RED=$(tput setaf 1)
_YELLOW=$(tput setaf 3)

git_prompt() {
    local BRANCH
    BRANCH=$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/*\(.*\)/\1/')

    if [ ! -z "$BRANCH" ]; then
        echo -n "(${_YELLOW}${BRANCH}${_RESET}"

        if [ ! -z "$(git status --short)" ]; then
            echo -n "${_RED}✗${_RESET}"
        fi
        echo -n ' ) '
    fi
}

PS1='$(git_prompt)'"$PS1"
