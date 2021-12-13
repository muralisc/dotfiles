#!/bin/bash

# this script is used to move footage from camera to 
# a destination folder with sub folders defined by
# yyyy-mm-dd/camera-model/file.jpg

# e.g: bash ~/src/dotfiles/bin/copy-footage.sh ~/data/footage/2021/2021_01_murali_mobile ~/data/footage/2021


SRC_FOLDER=$1
DST_FOLDER=${2:-.}
DRY_RUN=${3:-true}
for file_name in $(find $SRC_FOLDER -type f); do
  # path for the photo from the SRC_FOLDER
  PHOTO_PATH=${file_name/$SRC_FOLDER/}

  EXIF_INFO=$(exiftool -j $file_name)
  CREATE_DATE=$(jq -r '.[0].CreateDate' <<< "$EXIF_INFO")
  CAMERA_NAME=$(jq -r '.[0].Model' <<< "$EXIF_INFO" | tr ' ' '_')
  if [[ $CAMERA_NAME = "null" ]]; then
    CAMERA_NAME="NoModelName"
  fi
  FOLDER_DATE=$(strptime --input-format "%Y:%m:%d %H:%M:%S%Z" "$CREATE_DATE" --format "%Y_%m_%d")
  # if strptime is not successfull, break
  STRPTIME_RETURN=$?
  if [[ $STRPTIME_RETURN -ne 0 ]]; then
    echo "Processing $file_name failed"
    echo strptime return : $STRPTIME_RETURN
    exit 1
  fi

  DST_PATH="${DST_FOLDER}/${FOLDER_DATE}/${CAMERA_NAME}/$PHOTO_PATH"
  mkdir -p "${DST_FOLDER}/${FOLDER_DATE}/${CAMERA_NAME}"
  echo mv $file_name $DST_PATH
  if [[ $DRY_RUN != true ]]; then
    mv $file_name $DST_PATH
  else
    echo "Dry running: No move performed"
  fi
done
