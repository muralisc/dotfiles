#!/bin/bash


# http://www.netinstructions.com/automating-picture-capture-using-webcams-on-linuxubuntu/

export DISPLAY=:0
for i in 1 2 3 4
do
dir=`who -b | awk '{print $3"_"$4}'`
mkdir -p ~/Downloads/tl/$dir
ffmpeg -f video4linux2 -i /dev/video0 -vframes 1 ~/Downloads/tl/$dir/$(date +\%Y-\%m-\%d-\%H-%M)_${i}_tl.jpg
import -window root ~/Downloads/tl/${dir}/$(date +%Y-%m-%d-%H-%M)_${i}_scrot.jpg
sleep 14s
done 
