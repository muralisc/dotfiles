#!/usr/bin/env bash
# List the N (default 10) passwords whose last git commit is oldest.
# Uses git history, since file mtimes are reset on clone/sync.
cd "${PASSWORD_STORE_DIR:-$HOME/.password-store}" || exit 1
n=${1:-10}

git log --no-renames --name-only --format='@%at' -- '*.gpg' |
awk '/^@/ {ts=substr($0,2); next} /\.gpg$/ && !seen[$0]++ {print ts "\t" $0}' |
while IFS=$'\t' read -r ts f; do
  [ -e "$f" ] && printf '%s\t%s\n' "$ts" "$f"   # skip deleted entries
done |
sort -n | head -n "$n" |
while IFS=$'\t' read -r ts f; do
  printf '%s  %s\n' "$(date -d "@$ts" +%F)" "${f%.gpg}"
done
