#!/bin/bash


# http://www.netinstructions.com/automating-picture-capture-using-webcams-on-linuxubuntu/

export DISPLAY=:0
# take 4 pics in a minute
NOPICS=0
for i in `seq 1 $NOPICS`
do
dir=`who -b | awk '{print $3"_"$4}'` #get log in time
mkdir -p ~/Downloads/tl/$dir
ffmpeg -f video4linux2 -i /dev/video0 -vframes 1 ~/Downloads/tl/$dir/$(date +\%Y-\%m-\%d-\%H-%M)_${i}_tl.jpg >> /tmp/tl.log
if [[ -a /dev/video0 ]]; then
    #if video device exites
    import -window root ~/Downloads/tl/${dir}/$(date +%Y-%m-%d-%H-%M)_${i}_scrot.jpg >> /tmp/tl.log
fi
sleep $((60/$NOPICS -1))s
done 
# vim: set tw=200 :
