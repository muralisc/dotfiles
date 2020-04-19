#!/bin/bash

for filename in $@ ; do
    ffmpeg -i $filename \
        -copy_unknown -map_metadata 0 -codec copy \
        -codec:v libx264 -pix_fmt yuv420p -crf 23 \
        -codec:a aac -vbr 4 \
        -preset medium ${filename%.*}_encoded.mp4
done
