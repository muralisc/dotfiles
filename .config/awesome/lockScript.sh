#!/bin/bash

######## Take a screenshot
# scrot $FINAL_PATH
######## Pixellate it 10x
# mogrify -scale 10% -scale 1000% $FINAL_PATH
# convert -resize 1920 bb9a5897307ea096bd66dd658dfa1d75.jpg wallpaperLock.png
######## 1920x1080
Wallpaper_folder=~/Dropbox/Quotataions_copy/
LIST_PATH=/var/tmp/wallpaper_list
FINAL_PATH=/tmp/wallpaperLock.png
touch $LIST_PATH
if [ `wc -l $LIST_PATH | cut -d " " -f1` -eq 0 ];
then
    # execution here means no lines in file
    # get the files and shuffle them into the wallpaper list
    # SHUFFLE
    # \find $Wallpaper_folder -iregex ".*\(txt\|png\|jpg\|svg\)" |shuf > $LIST_PATH
    # NO SHUFFLE
    \find $Wallpaper_folder -iregex ".*\(txt\|png\|jpg\|svg\)" > $LIST_PATH
fi
# Lock screen displaying this image.
FILE=`head -1 $LIST_PATH`
sed -i "1d" $LIST_PATH
echo "$FILE" > /tmp/lastLock.log

# check if file is not JPG or PNG make png.

# get Extension
filename=$(basename "$FILE")
extension="${filename##*.}"
# if svg
if [ "$extension" == "svg" ];
then
    inkscape $FILE -e $FINAL_PATH
    FILE=$FINAL_PATH
    echo "convert svg"
fi
# if txt
if [ "$extension" == "txt" ];
then
    pushd $Wallpaper_folder
    ./printSvgFromFile.py $FILE > /tmp/wallpconvert.svg
    inkscape /tmp/wallpconvert.svg -e $FINAL_PATH
    popd
    FILE=$FINAL_PATH
    echo "convert txt"
fi

# resolution management. 
XRES=1920
YRES=1080
res=`identify $FILE | awk '{print $3}'`
x=`echo $res | grep -Po "[0-9]+(?=x)"`
y=`echo $res | grep -Po "(?<=x)[0-9]+"`


if [[ ( $YRES != $y ) || ( $XRES != $x ) || "$extension" != "png" ]]; then
    # 1920x1080 means 1920 in width and 1080 height
    # x = 1920 y = 1080; x > y
    # as long as img has similar charecteristics ie x1 > y1
    # resize to x
    if (( "$y" > "$x" )) 
    then
        convert -resize $XRES  $FILE $FINAL_PATH
        echo "resized to x $x to $XRES" >> /tmp/lastLock.log
    else
        # else rezize to y
        echo "resized to y $y to $YRES" >> /tmp/lastLock.log
        convert -resize x${YRES} $FILE $FINAL_PATH
        color=`convert -resize 1x1 $FINAL_PATH txt:-| grep -Po '#[0-9A-F]+'`
        convert -size ${XRES}x${YRES} xc:"$color" /tmp/background.png
        composite -gravity Center $FINAL_PATH /tmp/background.png $FINAL_PATH
    fi
else
    cp $FILE $FINAL_PATH
fi

if [ -z $1 ] 
then
# use the file
i3lock -i $FINAL_PATH
# Turn the screen off after a delay.
sleep 2; pgrep i3lock && xset dpms force off
else
    feh $FINAL_PATH
fi
