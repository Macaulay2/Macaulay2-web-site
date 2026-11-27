#!/bin/bash
# Upload the RPMs built by the "Build RPM packages" workflow to the dnf
# repositories on the website and regenerate their metadata.
#
# The repositories live under Repositories/ on the server, one per release:
#
#   Fedora/<release>/       Fedora (e.g., Fedora/43)
#   Scientific/<release>/   RHEL and compatible distributions, built on
#                           AlmaLinux (e.g., Scientific/9)
#
# Users point dnf at them with Fedora/Macaulay2.repo or
# Scientific/Macaulay2.repo (install-rpm.sh does this for them).
#
# 1. Build the RPMs.  The workflow (.github/workflows/build-rpm.yml in the M2
#    repository) runs automatically when a release-* tag is pushed.  If that
#    fails, fix things on a branch (e.g., in a fork) and run it manually:
#
#      gh workflow run build-rpm.yml -R OWNER/M2 --ref BRANCH
#
#    and then find the ID of the run with:
#
#      gh run list -w build-rpm.yml -R OWNER/M2
#
# 2. Upload them:
#
#      Repositories/upload-rpms.sh [options] RUN-ID [HOST:/path/to/Repositories]
#
#    e.g., Repositories/upload-rpms.sh -u USER -R OWNER/M2 RUN-ID
#
#    This downloads the workflow's artifacts (into a temporary directory that
#    is removed afterwards), sorts the RPMs into the directories above, copies
#    them to the server with rsync, and runs createrepo there on each
#    directory that was updated.  Use -n first to check what would be
#    uploaded.  The -debuginfo and -debugsource packages are skipped, since
#    they're huge and nobody uses them.  Nothing on the server is deleted;
#    dnf will pick the newest version, but old packages can be removed by
#    hand (then run "createrepo --update ." in that directory).
#
# 3. Regenerate the HTML directory listings by running "make" in the top
#    level directory of the website on the server
#    (/var/www/www2.macaulay2.com on aeschylus.macaulay2.com).
#
# Requires gh (logged in), rsync, and ssh locally, and createrepo on the
# server.

set -euo pipefail

GITHUB_REPO="${GITHUB_REPO:-Macaulay2/M2}"
DEST="${DEST:-aeschylus.macaulay2.com:/var/www/www2.macaulay2.com/Repositories}"

if [[ -t 1 ]]; then
    tty_bold=$'\033[1m'; tty_blue=$'\033[1;34m'; tty_red=$'\033[1;31m'
    tty_reset=$'\033[0m'
else
    tty_bold=""; tty_blue=""; tty_red=""; tty_reset=""
fi

ohai()  { printf '%s==>%s %s%s%s\n' "$tty_blue" "$tty_reset" "$tty_bold" "$*" "$tty_reset"; }
abort() { printf '%sError%s: %s\n' "$tty_red" "$tty_reset" "$*" >&2; exit 1; }

usage() {
    cat <<EOF
Usage: $0 [options] RUN-ID [HOST:/path/to/Repositories]

Upload the RPMs from a GitHub Actions run to the Macaulay2 dnf repositories.
The destination defaults to $DEST.

Options:
  -u, --user USER        log in to the server as USER
  -R, --repo OWNER/REPO  repository the workflow ran in (default: $GITHUB_REPO)
  -n, --dry-run          download and sort the RPMs, but don't upload anything
  -h, --help             show this message
EOF
}

main() {
    local dry_run="" user="" args=()
    while [[ $# -gt 0 ]]; do
        case $1 in
            -R|--repo) [[ $# -ge 2 ]] || abort "$1 needs an argument"
                       GITHUB_REPO=$2; shift ;;
            -u|--user) [[ $# -ge 2 ]] || abort "$1 needs an argument"
                       user=$2; shift ;;
            -n|--dry-run) dry_run=1 ;;
            -h|--help) usage; exit 0 ;;
            -*) usage >&2; abort "unknown option: $1" ;;
            *) args+=("$1") ;;
        esac
        shift
    done
    [[ ${#args[@]} -eq 1 || ${#args[@]} -eq 2 ]] || { usage >&2; exit 1; }
    local run_id=${args[0]} dest=${args[1]-$DEST}
    [[ $dest == *:* ]] || abort "destination should look like HOST:/path/to/Repositories"
    local host=${dest%%:*} path=${dest#*:}
    if [[ -n $user ]]; then
        host=$user@${host#*@}
        dest=$host:$path
    fi

    for cmd in gh rsync ssh; do
        command -v "$cmd" >/dev/null || abort "$cmd is required"
    done

    # not local, since the EXIT trap runs after main returns
    tmp=$(mktemp -d "${TMPDIR:-/tmp}/upload-rpms.XXXXXX")
    trap 'rm -rf "$tmp"' EXIT

    ohai "Downloading artifacts from $GITHUB_REPO run $run_id"
    gh run download "$run_id" -R "$GITHUB_REPO" -D "$tmp/artifacts"

    # artifacts are named OS-DISTRIBUTION-RELEASE, e.g. ubuntu-24.04-arm-fedora-43
    local artifact name release dist dirs=()
    for artifact in "$tmp"/artifacts/*/; do
        name=$(basename "$artifact")
        release=${name##*-}
        case $name in
            *-fedora-*)    dist=Fedora ;;
            *-almalinux-*) dist=Scientific ;;
            *) abort "don't know where to put artifact $name" ;;
        esac
        mkdir -p "$tmp/repos/$dist/$release"
        [[ " ${dirs[*]-} " == *" $dist/$release "* ]] || dirs+=("$dist/$release")
        find "$artifact" -name '*.rpm' \
             ! -name '*-debuginfo-*' ! -name '*-debugsource-*' \
             -exec cp {} "$tmp/repos/$dist/$release/" \;
    done
    [[ ${#dirs[@]} -gt 0 ]] || abort "no artifacts found"

    local dir
    for dir in "${dirs[@]}"; do
        ohai "$dir"
        ls "$tmp/repos/$dir"
        ls "$tmp/repos/$dir"/*.rpm >/dev/null 2>&1 || abort "no RPMs for $dir"
    done

    if [[ -n $dry_run ]]; then
        ohai "Dry run; not uploading to $dest"
        return
    fi

    ohai "Uploading to $dest"
    # the directories on the server may belong to someone else, and only the
    # owner can set their times
    rsync -av --omit-dir-times "$tmp/repos/" "$dest/"

    ohai "Regenerating repository metadata on $host"
    # shellcheck disable=SC2029 # we want $path and $dirs expanded locally
    # --update only works if there's already a repository (i.e., not for a
    # new release)
    ssh "$host" "set -e; cd '$path'; for d in ${dirs[*]}; do
                     if [ -d \"\$d/repodata\" ]; then createrepo --update \"\$d\";
                     else createrepo \"\$d\"; fi; done"
}

main "$@"
