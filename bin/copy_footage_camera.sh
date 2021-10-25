# Arg1 : Path containing images
# Arg2 : Path to create date directories + copy images


SRC_PATH="${1:-$PWD}"
DST_PATH="${2}"
DRY_RUN="${3}"


if [[ -z $DST_PATH ]]; then
	echo "Missing destination path"
	exit
fi

NO_OF_FILES=$(ls $SRC_PATH/* | wc -l)
LOOP_CNT=1

for file in $SRC_PATH/* ; do
	EXIF_INFO=$(exiftool "$file")
	camera_name=$(echo "$EXIF_INFO" | grep "Camera Model Name" | awk -F ': ' '{print $2}' | tr -s ' ' '_')
	date=$(echo "$EXIF_INFO" | grep "^Create Date" | head -1 | awk -F ': ' '{print $2}' | cut -f1 -d' ' | tr -s ':' '_')
	year=$(cut -f1 -d'_' <<< $date)

	DST_FOLDER="$DST_PATH/$year/$date/$camera_name"
	CMD="mv $file $DST_FOLDER/$(basename $file)"

        # Print move command
	echo "($LOOP_CNT/$NO_OF_FILES) $CMD"

        if [[ -z $DRY_RUN ]] ; then
	  # Run command
	  mkdir -p $DST_FOLDER
	  $CMD
        fi
	((LOOP_CNT++))
done

# mkdir {gopro7,oneplus5t,samsungM30,obs_screenrecording}
