#!/bin/bash
# useful linux commands
# either useful bash functions, or notes

# source me!

battery() { acpi -i; }

# linux kernel
syscallnum() {
    [ -z "$0" ] && { echo "${FUNCNAME[0]} <syscall|syscall_number>"; return 1; }

    local platform
    platform="$(python -mplatform)"

    local unistd
    # this file also works: <prefix>/arch/x86/entry/syscalls/syscall_64.tbl
    echo "$platform" | grep -qi ubuntu &&
        unistd=/usr/src/linux-headers-$(uname -r)/include/uapi/asm-generic/unistd.h
    echo "$platform" | grep -qi fedora &&
        unistd=/usr/src/kernels/$(uname -r)/include/uapi/asm-generic/unistd.h
    [ -z "$unistd" ] && { echo "Unknown platform: $platform"; return 2; }

    if [[ "$1" =~ ^[0-9]+$ ]]; then
        grep -w "$1" "$unistd"
    else
        grep -w -B1 "sys_$1" "$unistd"
    fi
}
