#!/bin/bash

# Usage:
# bash $1 $source_root $dest_root 'find_path_regex'
# e.g:
#   bash ~/src/dotfiles/bin/footage_manual_convert.sh ~/data/footage ~/data/footage_converted '.*_09.*CR3' dryrun
#   bash ~/src/dotfiles/bin/footage_manual_convert.sh ~/data/footage ~/data/footage_converted '.*IMG_043.\.HEIC' dryrun
#   bash ~/src/dotfiles/bin/footage_manual_convert.sh ~/data/footage ~/data/footage_converted '.*2022/.*_1[01]_.*\(jpg\|JPG\|CR3\)' dryrun

SOURCE_ROOT="$1"
DESTINATION_ROOT="$2"
PATH_REGEX="$3"
DRY_RUN="$4"

echo find $SOURCE_ROOT -regex "$PATH_REGEX"


mkdir -p $DESTINATION_ROOT

for file_path in $(find $SOURCE_ROOT -type f -regex "$PATH_REGEX"); do
    echo "----------------------"
    echo "Converting: $file_path"
    FILE_DIRECTORY=$(dirname $file_path)
    filename=$(basename $file_path)
    filename_without_ext="${filename%.*}"
    extension="${filename##*.}"
    RELATIVE_PATH_TO_SOURCE_ROOT=${FILE_DIRECTORY#"$SOURCE_ROOT"}
    DEST_FILE_DIRECTORY="${DESTINATION_ROOT}$RELATIVE_PATH_TO_SOURCE_ROOT"
    mkdir -p $DEST_FILE_DIRECTORY
    DEST_FILE_PATH="${DEST_FILE_DIRECTORY}/${filename_without_ext}.jpg"
    if [[ -f $DEST_FILE_PATH ]] ; then
        continue
    fi
    echo "-> To desitnation: $DEST_FILE_PATH"
    if [[ -z "$DRY_RUN" ]]; then
        if [[ "$extension" == "HEIC" ]] ; then
            convert -define jpeg:extent=400Kb $file_path $DEST_FILE_PATH
        else
            # rawtherapee-cli -j40 -js1 -o $DEST_FILE_PATH -c $file_path
            convert -define jpeg:extent=400Kb $file_path $DEST_FILE_PATH
            exiftool -overwrite_original_in_place -tagsFromFile $file_path $DEST_FILE_PATH
              if exiftool $DEST_FILE_PATH | grep 'Create Date' ; then
                  :
              else
                  echo "Converted File do not have DATE, adding it" $file_path
                  # Sometimes source file do not have Date info, in that case infer from foldername
                  DATE_FROM_PATH="$(awk -F'footage' '{print $2}' <<< "$file_path" | cut -d'/' -f 3)"
                  MAYBE_DATE=$(strptime --input-format "%Y_%m_%d" "$DATE_FROM_PATH" --format "%Y:%m:%d 00:00:00")
                  exiftool -overwrite_original "-AllDates=$MAYBE_DATE" $DEST_FILE_PATH
              fi
        fi
    fi
    # leaving the convert command below for future expansions
done
