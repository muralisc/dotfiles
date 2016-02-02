#!/bin/sh -e

######## Take a screenshot
# scrot /tmp/wallpaperLock.png
######## Pixellate it 10x
# mogrify -scale 10% -scale 1000% /tmp/wallpaperLock.png
# convert -resize 1920 bb9a5897307ea096bd66dd658dfa1d75.jpg wallpaperLock.png
######## 1920x1080
Wallpaper_folder=~/PersonalData_max26gb/Jpegs/Quotataions/
touch /var/tmp/wallpaper_list
if [ `wc -l /var/tmp/wallpaper_list | cut -d " " -f1` -eq 0 ];
then
    # execution here means no lines in file

    # get the files and shuffle them into the wallpaper list
    # \find $Wallpaper_folder -iname "*.jpg" -o -iname "*.png" |shuf > /var/tmp/wallpaper_list
    \find $Wallpaper_folder -iregex ".*\(txt\|png\|jpg\|svg\)" |shuf > /var/tmp/wallpaper_list
fi
# Lock screen displaying this image.
FILE=`head -1 /var/tmp/wallpaper_list`
sed -i "1d" /var/tmp/wallpaper_list
echo "$FILE" > /tmp/lastLock

# check if file is not JPG or PNG make png.

# get Extension
filename=$(basename "$FILE")
extension="${filename##*.}"
# if svg
if [ "$extension" == "svg" ];
then
    inkscape $FILE -e /tmp/wallpaperLock.png
    FILE=/tmp/wallpaperLock.png
    echo "convert svg"
fi
# if txt
if [ "$extension" == "txt" ];
then
    pushd $Wallpaper_folder
    ./printSvgFromFile.py $FILE > /tmp/wallpconvert.svg
    inkscape /tmp/wallpconvert.svg -e /tmp/wallpaperLock.png
    popd
    FILE=/tmp/wallpaperLock.png
    echo "convert txt"
fi

# resolution management. 
res=`identify $FILE | awk '{print $3}'`
x=`echo $res | grep -Po "[0-9]+(?=x)"`
y=`echo $res | grep -Po "(?<=x)[0-9]+"`
# 1920x1080 means 1920 in width and 1080 height
# x = 1920 y = 1080; x > y
# as long as img has similar charecteristics ie x1 > y1
# resize to x
if (( "$y" > "$x" )) 
then
    convert -resize 1920  $FILE /tmp/wallpaperLock.png
    echo "resized to x 1920" >> /tmp/lastLock
else
    # else rezize to y
    echo "resized to y 1080" >> /tmp/lastLock
    convert -resize x1080  $FILE /tmp/wallpaperLock.png
    color=`convert -resize 1x1 /tmp/wallpaperLock.png txt:-| grep -Po '#[0-9A-F]+'`
    convert -size 1920x1080 xc:"$color" background.png
    composite -gravity Center /tmp/wallpaperLock.png background.png /tmp/wallpaperLock.png
fi
# use the file
i3lock -i /tmp/wallpaperLock.png

# Turn the screen off after a delay.
sleep 60; pgrep i3lock && xset dpms force off
