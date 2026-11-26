#!/bin/bash
# Set up the Macaulay2 dnf repository and install Macaulay2 on Fedora,
# Red Hat Enterprise Linux, and RHEL-compatible distributions like Rocky
# Linux and AlmaLinux.  Usage:
#
#   /bin/bash -c "$(curl -fsSL https://macaulay2.com/Repositories/install-rpm.sh)"
#
# Options may be passed after a placeholder for $0, e.g.,
#
#   /bin/bash -c "$(curl -fsSL https://macaulay2.com/Repositories/install-rpm.sh)" _ --no-install
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
Set up the Macaulay2 dnf repository and install Macaulay2.

Options:
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

setup_repo() { # setup_repo REPO_URL
    ohai "Adding the Macaulay2 repository"
    tmp="$(mktemp)"
    trap 'rm -f "$tmp"' EXIT
    fetch "$1" "$tmp"
    run_root install -m 644 "$tmp" /etc/yum.repos.d/Macaulay2.repo
}

main() {
    local install=1
    NONINTERACTIVE="${NONINTERACTIVE:-}"
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --no-install) install="" ;;
            -y|--yes) NONINTERACTIVE=1 ;;
            -h|--help) usage; exit 0 ;;
            *) usage >&2; abort "unknown option: $1" ;;
        esac
        shift
    done

    [[ $(uname -s) == Linux ]] || abort "this script only works on Linux"
    command -v dnf >/dev/null ||
        abort "dnf not found; for Debian or Ubuntu, use ${M2_BASE_URL}/install-deb.sh instead"
    [[ -r /etc/os-release ]] || abort "/etc/os-release not found"
    # shellcheck disable=SC1091
    . /etc/os-release

    local id="${ID:-}" like=" ${ID_LIKE:-} " version="${VERSION_ID:-}" repo_url=""
    local -a plan
    if [[ $id == fedora ]]; then
        if [[ -n $version ]] &&
               url_exists "${M2_BASE_URL}/Fedora/${version}/repodata/repomd.xml"; then
            repo_url="${M2_BASE_URL}/Fedora/Macaulay2.repo"
            plan=("add the Macaulay2 repository for Fedora ${version}")
        else
            warn "no Macaulay2 repository for Fedora '${version:-unknown}'."
            warn "The Macaulay2 package from the official Fedora repositories will be used."
            [[ -n $install ]] || exit 0
            plan=()
        fi
    elif [[ $id == rhel || $like == *" rhel "* ]]; then
        version="${version%%.*}"
        url_exists "${M2_BASE_URL}/Scientific/${version}/repodata/repomd.xml" ||
            abort "no Macaulay2 repository for ${NAME:-$id} ${version}; see ${WIKI_URL}"
        repo_url="${M2_BASE_URL}/Scientific/Macaulay2.repo"
        plan=("add the Macaulay2 repository for ${NAME:-$id} ${version}")
    else
        abort "unsupported distribution '${id}'; see ${WIKI_URL}"
    fi
    [[ -n $install ]] && plan+=("install the Macaulay2 package")

    ohai "This script will:"
    printf -- '- %s\n' "${plan[@]}"
    confirm
    get_root

    [[ -n $repo_url ]] && setup_repo "$repo_url"

    if [[ -n $install ]]; then
        ohai "Installing Macaulay2"
        run_root dnf install -y Macaulay2
        ohai "Installation successful!  Run M2 to start Macaulay2."
    else
        ohai "Repository added.  Run 'sudo dnf install Macaulay2' to install Macaulay2."
    fi
}

main "$@"
