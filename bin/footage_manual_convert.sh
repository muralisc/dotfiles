#!/bin/bash

# Usage:
# bash $1 $source_root $dest_root 'find_path_regex'
# e.g:
#   bash ~/src/dotfiles/bin/footage_manual_convert.sh ~/data/footage ~/data/footage_converted '.*_09.*CR3' dryrun

SOURCE_ROOT="$1"
DESTINATION_ROOT="$2"
PATH_REGEX="$3"
DRY_RUN="$4"

echo find $SOURCE_ROOT -regex "$PATH_REGEX"


mkdir -p $DESTINATION_ROOT

for file_path in $(find $SOURCE_ROOT -regex "$PATH_REGEX"); do
    echo "----------------------"
    echo "Converting: $file_path"
    FILE_DIRECTORY=$(dirname $file_path)
    filename=$(basename $file_path)
    filename_without_ext="${filename%.*}"
    RELATIVE_PATH_TO_SOURCE_ROOT=${FILE_DIRECTORY#"$SOURCE_ROOT"}
    DEST_FILE_DIRECTORY="${DESTINATION_ROOT}$RELATIVE_PATH_TO_SOURCE_ROOT"
    mkdir -p $DEST_FILE_DIRECTORY
    DEST_FILE_PATH="${DEST_FILE_DIRECTORY}/${filename_without_ext}.jpg"
    echo "-> To desitnation: $DEST_FILE_PATH"
    if [[ -z "$DRY_RUN" ]]; then
        rawtherapee-cli -j40 -js1 -o $DEST_FILE_PATH -c $file_path
        exiftool -tagsFromFile $file_path $DEST_FILE_PATH
    fi
    # leaving the convert command below for future expansions
    # convert -define jpeg:extent=1Mb IMG_4140.CR3 converted/c.jpg
done

