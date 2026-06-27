#!/bin/bash

# Copies/moves media from a dump folder into YYYY/YYYY_MM_DD/CameraMake-Model/file hierarchy.
# Source may be from a camera, mobile phone, whatsapp, signal.
#
# Usage: footage-camera-2-storage-copy.sh <src> <dst> <dryrun|cp|mv> [DefaultCameraName]
# e.g:   bash ~/src/dotfiles/bin/media-organize/footage-camera-2-storage-copy.sh \
#             ~/data/footage/2021/2021_01_murali_mobile \
#             ~/data/footage/2021 cp "DefaultCameraName"

SRC_FOLDER=$1
DST_FOLDER=${2:-.}
COPY_COMMAND=${3}
DEFAULT_CAMERA_MAKE="${4:-NoModelName}"

if [[ -z $SRC_FOLDER ]]; then
  echo "Source folder is empty"
  exit 1
fi
if [[ -z $COPY_COMMAND ]]; then
  echo "Copy command is empty"
  echo "     dryrun - for dry run"
  echo "     cp     - for copy"
  echo "     mv     - for move"
  exit 1
fi

COUNT_OK=0
COUNT_SKIP=0
COUNT_FAIL=0
FAILED_FILES=()

while IFS= read -r -d '' file_name; do
  FILE_BASENAME=$(basename "$file_name")

  EXIF_INFO=$(exiftool -j "$file_name")

  # Extract all fields in one jq call (tab-separated)
  IFS=$'\t' read -r CREATE_DATE FILE_MODIFY_DATE CAMERA_MAKE CAMERA_MODEL_NAME < <(
    jq -r '[.[0].CreateDate // "null", .[0].FileModifyDate // "null", .[0].Make // "null", .[0].Model // "null"] | @tsv' <<< "$EXIF_INFO"
  )
  CAMERA_MAKE="${CAMERA_MAKE// /_}"
  CAMERA_MODEL_NAME="${CAMERA_MODEL_NAME// /_}"

  # Handle missing Make
  if [[ "$CAMERA_MAKE" == "null" ]]; then
    CAMERA_MAKE=""
  fi

  # Handle missing Model
  if [[ "$CAMERA_MODEL_NAME" == "null" ]]; then
    CAMERA_MODEL_NAME="$DEFAULT_CAMERA_MAKE"
  fi

  # Prepend make to model if not already present
  if [[ -n "$CAMERA_MAKE" && "$CAMERA_MODEL_NAME" != *"$CAMERA_MAKE"* ]]; then
    CAMERA_MODEL_NAME="${CAMERA_MAKE}-${CAMERA_MODEL_NAME}"
  fi

  # Parse date with CreateDate, fall back to FileModifyDate
  FOLDER_DATE=$(strptime --input-format "%Y:%m:%d %H:%M:%S%Z" "$CREATE_DATE" --format "%Y/%Y_%m_%d" 2>/dev/null)
  if [[ $? -ne 0 ]]; then
    echo "$(tput setaf 3)WARN: $file_name — CreateDate ($CREATE_DATE) invalid, trying FileModifyDate$(tput sgr0)"
    FOLDER_DATE=$(strptime --input-format "%Y:%m:%d %H:%M:%S%Z" "$FILE_MODIFY_DATE" --format "%Y/%Y_%m_%d" 2>/dev/null)
    if [[ $? -ne 0 ]]; then
      echo "$(tput setaf 1)FAIL: $file_name — FileModifyDate ($FILE_MODIFY_DATE) also invalid, skipping$(tput sgr0)"
      COUNT_FAIL=$((COUNT_FAIL + 1))
      FAILED_FILES+=("$file_name")
      continue
    fi
  fi

  DST_PATH="${DST_FOLDER}/${FOLDER_DATE}/${CAMERA_MODEL_NAME}/${FILE_BASENAME}"

  # Skip already-existing destination files
  if [[ -e "$DST_PATH" ]]; then
    echo "SKIP: $FILE_BASENAME already exists at $DST_PATH"
    COUNT_SKIP=$((COUNT_SKIP + 1))
    continue
  fi

  echo "$COPY_COMMAND  $file_name  ->  $DST_PATH"
  if [[ "$COPY_COMMAND" != "dryrun" ]]; then
    mkdir -p "${DST_FOLDER}/${FOLDER_DATE}/${CAMERA_MODEL_NAME}"
    $COPY_COMMAND -v "$file_name" "$DST_PATH"
  fi
  COUNT_OK=$((COUNT_OK + 1))

done < <(find "$SRC_FOLDER" -type f -print0)

echo ""
echo "Done: $COUNT_OK processed, $COUNT_SKIP skipped (already exist), $COUNT_FAIL failed"
if [[ ${#FAILED_FILES[@]} -gt 0 ]]; then
  echo "Failed files:"
  printf '  %s\n' "${FAILED_FILES[@]}"
fi
