#!/bin/bash
python -c '
from datetime import *;
print( (date( 2017,1,26) - date.today()).days )
' > /tmp/daycount.wallmaker
convert -background black -fill '#ffbbbbbb' -font Ubuntu  \
    -size 1920x1080 -gravity center \
    label:"`cat /tmp/daycount.wallmaker`" /tmp/`date +%F`.jpg
cp /tmp/`date +%F`.jpg /tmp/today.jpg
feh --bg-fill /tmp/today.jpg
