#!/bin/bash
# usage :
# ./stopwatch peak      to show current status
# ./stopwatch           to stop
if [ ! -f ~/stop.1 ];
then
    # file doesn not exits start stopwatch, and exit
    date +%s > ~/stop.1
    exit
fi
# in no parameter exists, stop
if [ -z $1 ];
then
    # file exists
    stop=`date +%s`
    start=`cat ~/stop.1`
    rm -rf ~/stop.1
    echo "   $(( ($stop - $start)/60 )) "
    echo "  `date -d @$start` "
    echo "  `date -d @$stop`  "
else
    # if a parameter exists, just show the curent status
    # if running else start
    stop=`date +%s`
    start=`cat ~/stop.1`
    echo "   $(( ($stop - $start)/60 )) "
    echo "  `date -d @$start` "
    echo "  `date -d @$stop`  "
    exit
fi

