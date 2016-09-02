#!/bin/bash
# usage :
# ./stopwatch peak      to show current status
# ./stopwatch           to stop
if [ ! -f /tmp/stop.1 ];
then
    if [ "$1" = "peak" ];
    then
        echo "not running"
        exit
    fi
    # file doesn not exits start stopwatch, and exit
    date +%s > /tmp/stop.1
    echo "start stopw"
    exit
fi
# in no parameter exists, stop
if [ -z $1 ];
then
    # control here means file exists
    stop=`date +%s`
    start=`cat /tmp/stop.1`
    rm -rf /tmp/stop.1
    echo "stop stopw"
    mins=$(( ($stop - $start)/60 ))
    secs=$(( ($stop - $start)-60*$mins ))
    echo " `date -I`  $mins:$secs" | tee >> ~/Dropbox/conctimes
    # echo "  `date -d @$start` "  #debug
    # echo "  `date -d @$stop`  "  #debug
else
    # if a parameter exists, just show the curent status
    # if running else start
    stop=`date +%s`
    start=`cat /tmp/stop.1`
    mins=$(( ($stop - $start)/60 ))
    secs=$(( ($stop - $start)-60*$mins ))
    echo " `date -I`  $mins:$secs"
    # echo "  `date -d @$start` "  #debug
    # echo "  `date -d @$stop`  "  #debug
    exit
fi
