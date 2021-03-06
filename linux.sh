#!/bin/bash
# useful linux commands
# either useful bash functions, or notes

# source me!

_platform="$(python -mplatform)"


battery() { acpi -i; }
alias cpufreq='watch -n1 "cat /proc/cpuinfo | grep \"^[c]pu MHz\""'

# start subshell with address layout randomization disabled
alias bash-no-rand='setarch $(uname -m) --addr-no-randomize /bin/bash -l'
# gdb disables address space randomization on Linux by default

# tcp
# sudo tcptrack -i wlp3s0
# dns
# sudo dnscap -i wlp3s0 -g


# linux kernel
_syscallnum_unistd() {
    local unistd=
    # this file also works: <prefix>/arch/x86/entry/syscalls/syscall_64.tbl
    echo "$_platform" | grep -qi ubuntu &&
        unistd=/usr/src/linux-headers-$(uname -r)/include/uapi/asm-generic/unistd.h
    echo "$_platform" | grep -qi -e fedora -e redhat &&
        unistd=/usr/src/kernels/$(uname -r)/include/uapi/asm-generic/unistd.h

    [ -z "$unistd" ] && { echo "Unknown platform: $_platform" >&2; return 2; }
    echo "$unistd"
}

syscallnum() {
    [ -z "$1" ] && { echo "${FUNCNAME[0]} <syscall|syscall_number>" >&2; return 1; }

    local unistd
    unistd=$(_syscallnum_unistd) || return 2

    if [[ "$1" =~ ^[0-9]+$ ]]; then
        grep -w "$1" "$unistd"
    else
        grep -w -B1 "sys_$1" "$unistd" || grep -B1 "$1" "$unistd"
    fi
}
