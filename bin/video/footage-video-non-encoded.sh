find ~/data/footage/2021/ -type f \
    -size +1G \
    -not \
    \( \
        -iregex '.*marriage_vid.*' -o \
        -iregex '.*\.tar.gz' -o \
        -iregex '.*\.tomb' -o \
        -iregex '.*\.JPG' -o \
        -iregex '.*\.PNG' -o \
        -iregex '.*\.mp3' -o \
        -iregex '.*\.HEIC' -o \
        -iregex '.*\.CR3' -o \
        -iregex '.*\.CR3\..*' -o \
        -iregex '.*\.MOV' -o \
        -iregex '.*\.VOB' -o \
        -iregex '.*Fast 1080p30.*m4v' -o \
        -iregex '.*Super HQ .*Surround.m4v' -o \
        -iregex '.*HQ .* Surround.m4v' -o \
        -iregex '.*custom-encode-script.MP4' -o \
        -regex '.*_encoded.*' \
    \) \
    -ls | \
    sort -r -n -k7 | \
    numfmt --field 7 --to=iec | tr -s ' ' | cut -f 8- -d ' '
