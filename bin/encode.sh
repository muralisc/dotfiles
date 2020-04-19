#!/bin/bash


$DEST_DIR=$1
shift
for filename in $@ ; do
    ffmpeg -i $filename                           \
        -copy_unknown                             \  # Copy Unknown streams
        -map_metadata 0                           \  # map metadat from inp file
        -codec copy                               \  # copy data streams
        -codec:v libx264 -pix_fmt yuv420p -crf 23 \  # encode video using libx264 with pix_fmt yuv420p and cfr of 23
        -codec:a aac -vbr 4                       \  # encode audio using aac adn vbr of 4
        -preset medium ${DEST_DIR}/${filename%.*}_encoded.mp4
done
