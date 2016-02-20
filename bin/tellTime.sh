#!/bin/bash
MINUTES=$1
SECONDS=$(( $MINUTES * 60))
echo "time told every $MINUTES == $SECONDS"
while true ;
do
    echo `date '+%X'`
    echo `date '+%X'` | festival --tts
    sleep $SECONDS;
done
