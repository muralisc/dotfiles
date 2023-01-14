#!/bin/bash

# this script is used to move footage from camera to 
# a destination folder with sub folders defined by
# yyyy-mm-dd/camera-model/file.jpg

# e.g: bash ~/src/dotfiles/bin/footage-copy.sh ~/data/footage/2021/2021_01_murali_mobile ~/data/footage/2021 "DefaultCameraName"


SRC_FOLDER=$1
DST_FOLDER=${2:-.}
COPY_COMMAND=${3} #dry run if not specified
DEFAULT_CAMERA_MAKE="${4:-NoModelName}"

if [[ -z $SRC_FOLDER ]]; then
  echo "Source folder is empty"
  exit
fi
if [[ -z $COPY_COMMAND ]]; then
  echo "Copy command is empty"
  echo "     dryrun - for dry run"
  echo "     cp - for copy"
  echo "     mv - for move"
  exit
fi

for file_name in $(find $SRC_FOLDER -type f); do
  # path for the photo from the SRC_FOLDER
  PHOTO_PATH=${file_name/$SRC_FOLDER/}

  EXIF_INFO=$(exiftool -j $file_name)
  CREATE_DATE=$(jq -r '.[0].CreateDate' <<< "$EXIF_INFO")
  FILE_MODIFY_DATE=$(jq -r '.[0].FileModifyDate' <<< "$EXIF_INFO")
  CAMERA_MAKE=$(jq -r '.[0].Make' <<< "$EXIF_INFO" | tr ' ' '_')
  CAMERA_MODEL_NAME=$(jq -r '.[0].Model' <<< "$EXIF_INFO" | tr ' ' '_')
  if [[ $CAMERA_MODEL_NAME = "null" ]]; then
    CAMERA_MODEL_NAME=$DEFAULT_CAMERA_MAKE
  fi
  if ! grep $CAMERA_MAKE <<< $CAMERA_MODEL_NAME > /dev/null ; then
    CAMERA_MODEL_NAME=${CAMERA_MAKE}-${CAMERA_MODEL_NAME}
  fi
  FOLDER_DATE=$(strptime --input-format "%Y:%m:%d %H:%M:%S%Z" "$CREATE_DATE" --format "%Y_%m_%d")
  # if strptime is not successfull, break
  STRPTIME_RETURN=$?
  if [[ $STRPTIME_RETURN -ne 0 ]]; then
    echo "$(tput setaf 2)Processing $file_name failed as CREATE_DATE (val: $CREATE_DATE) is not correct format$(tput sgr0)"
    echo strptime return : $STRPTIME_RETURN
    echo "Trying with FileModifyDate"

    FOLDER_DATE=$(strptime --input-format "%Y:%m:%d %H:%M:%S%Z" "$FILE_MODIFY_DATE" --format "%Y_%m_%d")
    STRPTIME_RETURN=$?
    if [[ $STRPTIME_RETURN -ne 0 ]]; then
      echo "$(tput setaf 1)Processing $file_name failed as FILE_MODIFY_DATE (val: $FILE_MODIFY_DATE) is not correct format$(tput sgr0)"
      echo strptime return : $STRPTIME_RETURN
      exit 1
    fi
  fi

  DST_PATH="${DST_FOLDER}/${FOLDER_DATE}/${CAMERA_MODEL_NAME}/$PHOTO_PATH"
  mkdir -p "${DST_FOLDER}/${FOLDER_DATE}/${CAMERA_MODEL_NAME}"
  echo $COPY_COMMAND $file_name $DST_PATH
  if [[ $COPY_COMMAND != "dryrun" ]]; then
    $COPY_COMMAND -v $file_name $DST_PATH
  else
    echo "Copy command is $COPY_COMMAND, Dry running: No move performed"
  fi
done
