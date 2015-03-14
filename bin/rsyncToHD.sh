#!/bin/bash
rsync -ah \
    --info=copy,del,progress2,stats\
    --delete \
    --exclude='.cache'\
    --exclude='.local'\
    --exclude='.thumbnails/'\
    ./ /media/MurHD/

