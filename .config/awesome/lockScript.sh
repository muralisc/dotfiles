#!/bin/sh -e

######## Take a screenshot
# scrot ~/Downloads/wallpaperLock.png
######## Pixellate it 10x
# mogrify -scale 10% -scale 1000% ~/Downloads/wallpaperLock.png
# convert -resize 1920 bb9a5897307ea096bd66dd658dfa1d75.jpg wallpaperLock.png
######## 1920x1080
if [ `wc -l ~/Downloads/wallpaper_list | cut -d " " -f1` -eq 0 ];
then
    # execution here means no lines in file

    # get the files and shuffle them into the wallpaper list
    \find ~/PersonalData_max26gb/Wallpaper/Widescreen_HD_Nature -iname "*.jpg" -o -iname "*.png" |shuf > ~/Downloads/wallpaper_list
fi
# Lock screen displaying this image.
FILE=`head -1 ~/Downloads/wallpaper_list`
sed -i "1d" ~/Downloads/wallpaper_list
echo "$FILE" > ~/Downloads/lastLock
convert -resize 1920  $FILE ~/Downloads/wallpaperLock.png
i3lock -i ~/Downloads/wallpaperLock.png

# Turn the screen off after a delay.
sleep 60; pgrep i3lock && xset dpms force off
