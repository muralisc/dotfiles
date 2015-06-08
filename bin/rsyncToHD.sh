#!/bin/bash
rsync -aPvh \
    --info=copy,del,progress2,stats\
    --delete \
    --exclude='.*'\
    --exclude='.cache'\
    --exclude='.config/google-chrome/'\
    --exclude='.local'\
    --exclude='.thumbnails/'\
    /home/murali/ /run/media/murali/MurHD/

