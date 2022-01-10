#!/bin/bash

# Script to encode multiple files to mp4

# Command details:
#  time ffmpeg -i $filename                       \
#      -copy_unknown                              \ # Copy Unknown streams
#      -map_metadata 0                            \ # map metadat from inp file
#      -codec copy                                \ # copy data streams
#      -codec:v libx264 -pix_fmt yuvj420p -crf 23 \ # encode video using libx264 with pix_fmt yuv420p and cfr of 23
#      -codec:a aac -vbr 4                        \ # encode audio using aac adn vbr of 4
#      -preset medium ${DEST_DIR}/${filename%.*}_encoded.mp4

DEST_DIR=${1:-$PWD}
shift
for filename in $@ ; do
    echo "Starting to encode File: $filename at $(date):"
    time ffmpeg -i "$filename"                       \
        -copy_unknown                              \
        -map_metadata 0                            \
        -codec copy                                \
        -codec:v libx264 -pix_fmt yuvj420p -crf 23 \
        -codec:a aac -vbr 4                        \
        -preset fast "${DEST_DIR}/${filename%.*}_encoded.mp4"
done
