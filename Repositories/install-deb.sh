#!/bin/bash
# Set up the Macaulay2 apt repository and install Macaulay2 on Debian,
# Ubuntu, and their derivatives.  Usage:
#
#   /bin/bash -c "$(curl -fsSL https://macaulay2.com/Repositories/install-deb.sh)"
#
# Options may be passed after a placeholder for $0, e.g.,
#
#   /bin/bash -c "$(curl -fsSL https://macaulay2.com/Repositories/install-deb.sh)" _ --dev
#
# See https://github.com/Macaulay2/M2/wiki/Installing-Macaulay2-in-Linux

# everything is wrapped in functions so that nothing runs if the download
# is interrupted partway through
set -euo pipefail

M2_BASE_URL="${M2_BASE_URL:-https://macaulay2.com/Repositories}"
WIKI_URL="https://github.com/Macaulay2/M2/wiki/Installing-Macaulay2-in-Linux"

if [[ -t 1 ]]; then
    tty_bold=$'\033[1m'; tty_blue=$'\033[1;34m'; tty_red=$'\033[1;31m'
    tty_yellow=$'\033[1;33m'; tty_reset=$'\033[0m'
else
    tty_bold=""; tty_blue=""; tty_red=""; tty_yellow=""; tty_reset=""
fi

ohai()  { printf '%s==>%s %s%s%s\n' "$tty_blue" "$tty_reset" "$tty_bold" "$*" "$tty_reset"; }
warn()  { printf '%sWarning%s: %s\n' "$tty_yellow" "$tty_reset" "$*" >&2; }
abort() { printf '%sError%s: %s\n' "$tty_red" "$tty_reset" "$*" >&2; exit 1; }

usage() {
    cat <<EOF
Set up the Macaulay2 apt repository and install Macaulay2.

Options:
  --dev          on Ubuntu, use the PPA with the latest development version
  --no-install   only set up the repository; don't install Macaulay2
  -y, --yes      don't ask for confirmation (or set NONINTERACTIVE=1)
  -h, --help     show this message
EOF
}

fetch() { # fetch URL [FILE]
    if command -v curl >/dev/null; then
        curl -fsSL --retry 3 -o "${2:-/dev/stdout}" "$1"
    elif command -v wget >/dev/null; then
        wget -qO "${2:--}" "$1"
    else
        abort "curl or wget is required"
    fi
}

url_exists() {
    if command -v curl >/dev/null; then
        curl -fsIL --retry 3 -o /dev/null "$1"
    else
        wget -q --spider "$1"
    fi
}

run_root() { if [[ -n $SUDO ]]; then sudo "$@"; else "$@"; fi; }

get_root() {
    if [[ $EUID -eq 0 ]]; then
        SUDO=""
    elif command -v sudo >/dev/null; then
        SUDO=sudo
        if [[ -n $NONINTERACTIVE ]]; then
            sudo -n -v 2>/dev/null ||
                abort "sudo requires a password; rerun interactively or as root"
        else
            ohai "Root privileges are needed to add the repository and install packages."
            sudo -v || abort "sudo failed"
        fi
    else
        abort "root privileges are required; rerun as root or install sudo"
    fi
}

confirm() {
    [[ -z $NONINTERACTIVE && -t 0 ]] || return 0
    local c
    printf 'Press %sRETURN%s to continue or any other key to abort: ' "$tty_bold" "$tty_reset"
    IFS= read -r -s -n 1 c || abort "aborted"
    echo
    [[ -z $c ]] || abort "aborted"
}

apt_get() { run_root env DEBIAN_FRONTEND=noninteractive apt-get "$@"; }

setup_ppa() { # setup_ppa PPA CODENAME
    local ppa="$1" codename="$2"
    if ! url_exists "https://ppa.launchpadcontent.net/${ppa}/ubuntu/dists/${codename}/Release"; then
        warn "ppa:${ppa} has no packages for '${codename}'."
        abort "your release may no longer be supported; see ${WIKI_URL}"
    fi
    ohai "Adding ppa:${ppa}"
    if ! command -v add-apt-repository >/dev/null; then
        apt_get update
        apt_get install -y software-properties-common
    fi
    run_root add-apt-repository -y "ppa:${ppa}"
}

setup_debian() { # setup_debian SOURCES_URL
    local keyring
    ohai "Adding the Macaulay2 repository"
    apt_get update
    apt_get install -y ca-certificates
    tmp="$(mktemp)"
    trap 'rm -f "$tmp"' EXIT
    fetch "$1" "$tmp"
    run_root install -m 644 "$tmp" /etc/apt/sources.list.d/macaulay2.sources
    # older releases keep the key in a separate file rather than inline
    keyring="$(sed -n 's|^Signed-By: *\(/.*\)$|\1|p' "$tmp")"
    if [[ -n $keyring ]]; then
        fetch "${M2_BASE_URL}/Debian/$(basename "$keyring")" "$tmp"
        run_root install -D -m 644 "$tmp" "$keyring"
    fi
}

main() {
    local dev="" install=1
    NONINTERACTIVE="${NONINTERACTIVE:-}"
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --dev) dev=1 ;;
            --no-install) install="" ;;
            -y|--yes) NONINTERACTIVE=1 ;;
            -h|--help) usage; exit 0 ;;
            *) usage >&2; abort "unknown option: $1" ;;
        esac
        shift
    done

    [[ $(uname -s) == Linux ]] || abort "this script only works on Linux"
    command -v apt-get >/dev/null ||
        abort "apt-get not found; for Fedora or RHEL, use ${M2_BASE_URL}/install-rpm.sh instead"
    [[ -r /etc/os-release ]] || abort "/etc/os-release not found"
    # shellcheck disable=SC1091
    . /etc/os-release

    local id="${ID:-}" like=" ${ID_LIKE:-} " codename sources_url=""
    local -a plan
    if [[ $id == ubuntu || $like == *" ubuntu "* ]]; then
        codename="${UBUNTU_CODENAME:-${VERSION_CODENAME:-}}"
        [[ -n $codename ]] || abort "could not determine the Ubuntu release"
        local ppa="macaulay2/macaulay2"
        [[ -n $dev ]] && ppa="profzoom/macaulay2"
        plan=("add ppa:${ppa} for Ubuntu ${codename}")
    elif [[ $id == debian || $like == *" debian "* ]]; then
        [[ -z $dev ]] || warn "--dev is only supported on Ubuntu; ignoring"
        codename="${DEBIAN_CODENAME:-${VERSION_CODENAME:-}}"
        if [[ -n $codename ]] &&
               url_exists "${M2_BASE_URL}/Debian/${codename}/macaulay2.sources"; then
            sources_url="${M2_BASE_URL}/Debian/${codename}/macaulay2.sources"
            plan=("add the Macaulay2 repository for Debian ${codename}")
        else
            warn "no Macaulay2 repository for Debian '${codename:-unknown}'."
            warn "The macaulay2 package from the official Debian repositories will be used."
            [[ -n $install ]] || exit 0
            plan=()
        fi
    else
        abort "unsupported distribution '${id}'; see ${WIKI_URL}"
    fi
    [[ -n $install ]] && plan+=("install the macaulay2 package")

    ohai "This script will:"
    printf -- '- %s\n' "${plan[@]}"
    confirm
    get_root

    if [[ $id == ubuntu || $like == *" ubuntu "* ]]; then
        setup_ppa "$ppa" "$codename"
    elif [[ -n $sources_url ]]; then
        setup_debian "$sources_url"
    fi

    if [[ -n $install ]]; then
        ohai "Installing Macaulay2"
        apt_get update
        apt_get install -y macaulay2
        ohai "Installation successful!  Run M2 to start Macaulay2."
    else
        ohai "Repository added.  Run 'sudo apt install macaulay2' to install Macaulay2."
    fi
}

main "$@"
