FOOTAGE_PATH="$HOME/data/footage/2021"
FOLDER_OLD=$(dateadd now -1y --format "%Y_%m")

TMP_THUMB_DIR=/tmp/$(date -I)
mkdir -p $TMP_THUMB_DIR
for photo in $(find $FOOTAGE_PATH -ipath "*${FOLDER_OLD}*" -iname "*jpg" | head -15); do 
	echo "Converting $photo to thumb" 
	convert -verbose -thumbnail 160 $photo $TMP_THUMB_DIR/thumb-$(basename $photo); 
done
neomutt -a /tmp/$TMP_THUMB_DIR/thumb-* -s "Memories from 1 year ago : $FOLDER_OLD" -- muralisc@gmail.com <<< "Old Photo memories"
