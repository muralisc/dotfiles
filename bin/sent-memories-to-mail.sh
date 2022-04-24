FOOTAGE_PATH="$HOME/data/footage/2021"
FOLDER_OLD=$(dateadd now -1y --format "%Y_%m")

TMP_THUMB_DIR=/tmp/thub_$(date -I)
EMAIL_BODY_FILE=${TMP_THUMB_DIR}/body.txt
mkdir -p $TMP_THUMB_DIR
echo "Old Photo memories" > $EMAIL_BODY_FILE
find $FOOTAGE_PATH -ipath "*${FOLDER_OLD}*" |\
	awk -F'/' '{print $6" "$7}' |\
	uniq -c | awk '{print $1" http://192.168.1.237:8888/gallery/"$2"%2F"$3}' >> $EMAIL_BODY_FILE
for photo in $(find $FOOTAGE_PATH -ipath "*${FOLDER_OLD}*" -iname "*jpg" | shuf | head -5); do 
	echo "Converting $photo to thumb" 
	convert -verbose -thumbnail 160 $photo $TMP_THUMB_DIR/thumb-$(basename $photo); 
done
neomutt -a $TMP_THUMB_DIR/thumb-* -s "Memories from 1 year ago : $FOLDER_OLD" -- muralisc@gmail.com < $EMAIL_BODY_FILE
