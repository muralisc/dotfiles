find ~/data/footage/ -type f -ls | \
    sort -r -n -k7 | \
    numfmt --field 7 --to=iec
