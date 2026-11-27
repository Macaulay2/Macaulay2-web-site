#!/bin/bash
# Upload the Debian packages built by the "Build Debian packages" workflow to
# the apt repository on the website and sign its Release files.
#
# The repository is a flat mini-dinstall archive under Repositories/Debian on
# the server, with one directory per release (e.g., Debian/bookworm and
# Debian/trixie).  Users point apt at it with Debian/<release>/macaulay2.sources
# (install-deb.sh does this for them).
#
# 1. Build the packages.  The packaging lives on Salsa
#    (https://salsa.debian.org/math-team/macaulay2), with one branch per
#    release (e.g., debian/trixie).  Backport the debian/latest branch to each
#    of them and push.  Then run the workflow (.github/workflows/build-deb.yml
#    in the M2 repository):
#
#      gh workflow run build-deb.yml -R OWNER/M2
#
#    and then find the ID of the run with:
#
#      gh run list -w build-deb.yml -R OWNER/M2
#
# 2. Upload them:
#
#      Repositories/upload-debs.sh [options] RUN-ID [HOST:/path/to/Repositories/Debian]
#
#    e.g., Repositories/upload-debs.sh -u USER -R OWNER/M2 RUN-ID
#
#    This downloads the workflow's artifacts (into a temporary directory that
#    is removed afterwards), signs the .changes files with debsign, uploads
#    them with dput to the mini-dinstall incoming directory on the server, and
#    runs "mini-dinstall -b -v" there.  The release each package goes into
#    comes from the Distribution field of its .changes file.  Use -n first to
#    check what would be uploaded.
#
#    Finally, it signs the Release file of each release (mini-dinstall
#    regenerates all of them, even those with no new packages) without the private key ever leaving this machine: Release is fetched
#    over ssh, clearsigned locally, and sent back as InRelease.  This is
#    based on the sign-remote script from
#    https://upsilon.cc/~zack/blog/posts/2009/04/howto:_uploading_to_people.d.o_using_dput/
#
# 3. Regenerate the HTML directory listings by running "make" in the top
#    level directory of the website on the server
#    (/var/www/www2.macaulay2.com on aeschylus.macaulay2.com).
#
# Requires gh (logged in), debsign (devscripts), dput, gpg (with the signing
# key), scp, and ssh locally, and mini-dinstall (configured in
# ~/.mini-dinstall.conf of the account used to log in) on the server.

set -euo pipefail

GITHUB_REPO="${GITHUB_REPO:-Macaulay2/M2}"
DEST="${DEST:-aeschylus.macaulay2.com:/var/www/www2.macaulay2.com/Repositories/Debian}"

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
Usage: $0 [options] RUN-ID [HOST:/path/to/Repositories/Debian]

Upload the Debian packages from a GitHub Actions run to the Macaulay2 apt
repository and sign its Release files.
The destination defaults to $DEST.

Options:
  -u, --user USER        log in to the server as USER
  -R, --repo OWNER/REPO  repository the workflow ran in (default: $GITHUB_REPO)
  -k, --key KEYID        sign with KEYID (default: DEBSIGN_KEYID if set,
                         otherwise debsign's and gpg's defaults)
  -n, --dry-run          download and sort the packages, but don't sign or
                         upload anything
  -h, --help             show this message
EOF
}

main() {
    local dry_run="" user="" key="${DEBSIGN_KEYID-}" args=()
    while [[ $# -gt 0 ]]; do
        case $1 in
            -R|--repo) [[ $# -ge 2 ]] || abort "$1 needs an argument"
                       GITHUB_REPO=$2; shift ;;
            -u|--user) [[ $# -ge 2 ]] || abort "$1 needs an argument"
                       user=$2; shift ;;
            -k|--key)  [[ $# -ge 2 ]] || abort "$1 needs an argument"
                       key=$2; shift ;;
            -n|--dry-run) dry_run=1 ;;
            -h|--help) usage; exit 0 ;;
            -*) usage >&2; abort "unknown option: $1" ;;
            *) args+=("$1") ;;
        esac
        shift
    done
    [[ ${#args[@]} -eq 1 || ${#args[@]} -eq 2 ]] || { usage >&2; exit 1; }
    local run_id=${args[0]} dest=${args[1]-$DEST}
    [[ $dest == *:* ]] || abort "destination should look like HOST:/path/to/Repositories/Debian"
    local host=${dest%%:*} path=${dest#*:}
    if [[ -n $user ]]; then
        host=$user@${host#*@}
        dest=$host:$path
    fi

    for cmd in gh debsign dput gpg scp ssh; do
        command -v "$cmd" >/dev/null || abort "$cmd is required"
    done

    # not local, since the EXIT trap runs after main returns
    tmp=$(mktemp -d "${TMPDIR:-/tmp}/upload-debs.XXXXXX")
    trap 'rm -rf "$tmp"' EXIT

    ohai "Downloading artifacts from $GITHUB_REPO run $run_id"
    gh run download "$run_id" -R "$GITHUB_REPO" -D "$tmp/artifacts"

    # each artifact has the binary .changes file for one architecture and
    # release, along with the .debs and .buildinfo it lists.  It also has the
    # source .changes file, but not the source package itself, so skip that.
    local changes=() releases=() file release
    while IFS= read -r -d '' file; do
        changes+=("$file")
        release=$(sed -n 's/^Distribution: *//p' "$file")
        [[ -n $release ]] || abort "no Distribution field in $(basename "$file")"
        [[ " ${releases[*]-} " == *" $release "* ]] || releases+=("$release")
    done < <(find "$tmp/artifacts" -name '*.changes' ! -name '*_source.changes' -print0 | sort -z)
    [[ ${#changes[@]} -gt 0 ]] || abort "no .changes files found"

    for release in "${releases[@]}"; do
        ohai "$release"
        for file in "${changes[@]}"; do
            if grep -qx "Distribution: *$release" "$file"; then
                echo "$(basename "$file"):"
                # the files it lists (debs and buildinfo)
                sed -n '/^Files:/,/^[^ ]/s/^ .* //p' "$file" | sed 's/^/  /'
            fi
        done
    done

    if [[ -n $dry_run ]]; then
        ohai "Dry run; not uploading to $dest"
        return
    fi

    ohai "Signing .changes files"
    debsign --no-re-sign ${key:+-k "$key"} "${changes[@]}"

    ohai "Uploading to $host"
    # write our own dput configuration so that ~/.dput.cf isn't needed
    local login="*"
    [[ $host == *@* ]] && login=${host%%@*}
    cat > "$tmp/dput.cf" <<EOF
[macaulay2]
fqdn = ${host#*@}
login = $login
incoming = $path/mini-dinstall/incoming/
method = scp
allow_unsigned_uploads = 0
EOF
    dput -c "$tmp/dput.cf" macaulay2 "${changes[@]}"

    ohai "Running mini-dinstall on $host"
    ssh "$host" mini-dinstall -b -v

    # mini-dinstall regenerates the Release file of every release, not just
    # the ones we uploaded to, so sign all of them
    local dir signed=()
    # shellcheck disable=SC2029 # we want $path expanded locally
    mapfile -t signed < <(ssh "$host" "cd '$path' && ls -d */Release" | sed 's|/Release$||')
    [[ ${#signed[@]} -gt 0 ]] || abort "no Release files found in $path"
    for release in "${signed[@]}"; do
        ohai "Signing $release/Release"
        dir=$tmp/sign/$release
        mkdir -p "$dir"
        # shellcheck disable=SC2029 # we want $path and $release expanded locally
        ssh "$host" "cat '$path/$release/Release'" > "$dir/Release"
        [[ -s $dir/Release ]] || abort "couldn't fetch $release/Release"
        gpg ${key:+--local-user "$key"} --clearsign -o "$dir/InRelease" "$dir/Release"
        gpg --verify "$dir/InRelease"
        # write to a temporary file first so apt never sees a partial InRelease
        # shellcheck disable=SC2029
        ssh "$host" "cat > '$path/$release/InRelease.new' &&
                     mv '$path/$release/InRelease.new' '$path/$release/InRelease'" \
            < "$dir/InRelease"
    done
}

main "$@"
