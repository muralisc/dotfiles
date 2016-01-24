#!/bin/sh -e

######## Take a screenshot
# scrot ~/Downloads/wallpaperLock.png
######## Pixellate it 10x
# mogrify -scale 10% -scale 1000% ~/Downloads/wallpaperLock.png
# convert -resize 1920 bb9a5897307ea096bd66dd658dfa1d75.jpg wallpaperLock.png
######## 1920x1080
Wallpaper_folder=~/PersonalData_max26gb/Jpegs/Wallpapers/misc
touch ~/Downloads/wallpaper_list
if [ `wc -l ~/Downloads/wallpaper_list | cut -d " " -f1` -eq 0 ];
then
    # execution here means no lines in file

    # get the files and shuffle them into the wallpaper list
    \find $Wallpaper_folder -iname "*.jpg" -o -iname "*.png" |shuf > ~/Downloads/wallpaper_list
fi
# Lock screen displaying this image.
FILE=`head -1 ~/Downloads/wallpaper_list`
sed -i "1d" ~/Downloads/wallpaper_list
echo "$FILE" > ~/Downloads/lastLock

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
    convert -resize 1920  $FILE ~/Downloads/wallpaperLock.png
    echo "resized to x 1920" >> ~/Downloads/lastLock
else
    # else rezize to y
    echo "resized to y 1080" >> ~/Downloads/lastLock
    convert -resize x1080  $FILE ~/Downloads/wallpaperLock.png
    color=`convert -resize 1x1 ~/Downloads/wallpaperLock.png txt:-| grep -Po '#[0-9A-F]+'`
    convert -size 1920x1080 xc:"$color" background.png
    composite -gravity Center ~/Downloads/wallpaperLock.png background.png ~/Downloads/wallpaperLock.png
fi
# use the file
i3lock -i ~/Downloads/wallpaperLock.png

# Turn the screen off after a delay.
sleep 60; pgrep i3lock && xset dpms force off
