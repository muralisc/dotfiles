#!/bin/bash

function setMinsSecs {
    start=$1;
    stop=`date +%s`
    hrs=$((    ($stop - $start)/3600        ))
    mins=$(( ( ($stop - $start)%3600 ) /60  ))
    secs=$((   ($stop - $start)%60          ))
}
# echo "  `date -d @$start` "  #debug
# echo "  `date -d @$stop`  "  #debug

function speakDelta {
    if [ "$hrs" -gt "0" ]; then
        echo "$hrs hours $mins minutes " | xargs -I'{}' espeak '{}' 2> /dev/null
    elif [ "$mins" -gt "0" ]; then
        echo "$mins minutes" | xargs -I'{}' espeak '{}' 2> /dev/null
    else
        echo "$secs seconds" | xargs -I'{}' espeak '{}' 2> /dev/null
    fi
}

if [ ! -f /tmp/stopwatch.turnon ];
then
    date +%s > /tmp/stopwatch.turnon
    date -d today10pm +%s > /tmp/stopwatch.10pm
fi

if [ -z $1 ]; then
    echo "Usage: "
    echo "      start"
    echo "      stop"
    echo "      toggle "
    echo "      peak "
    echo "      interrupt"
    echo "      tell"
else
    case "$1" in
        toggle)
            if [ ! -f /tmp/stopwatch.start ];
            then
                # call itself
                bash $0 start
            else
                bash $0 stop
            fi
            ;;
        start)
            date +%s > /tmp/stopwatch.interrupt
            bash $0 tell

            date +%s > /tmp/stopwatch.start
            espeak "starting at time" 2> /dev/null
            date +'%I:%M %p' | espeak 2> /dev/null
            ;;
        stop)
            date +%s > /tmp/stopwatch.interrupt
            setMinsSecs `cat /tmp/stopwatch.start`
            speakDelta
            espeak "stopping" 2> /dev/null

            rm /tmp/stopwatch.start
            echo "$mins:$secs" | tee >> /var/tmp/intrruptimes.`date -I`
            echo " $mins:$secs" | tee >> /var/tmp/conctimes.`date -I`
            ;;
        interrupt)
            if [ ! -f /tmp/stopwatch.start ];
            then
                espeak "not running" 2> /dev/null
                bash $0 tell; # since last activity
                exit;
            fi
            # time since interrupt
            setMinsSecs `cat /tmp/stopwatch.interrupt`
            speakDelta
            # time sinnce begining
            setMinsSecs `cat /tmp/stopwatch.start`
            speakDelta


            date +%s > /tmp/stopwatch.interrupt
            echo "$mins:$secs" | tee >> /var/tmp/intrruptimes.`date -I`
            ;;
        tell)
            # time since last activity
            setMinsSecs `cat /tmp/stopwatch.start || cat /tmp/stopwatch.interrupt || cat /tmp/stopwatch.turnon || date +%s`
            speakDelta
            ;;
        print) # only printing
            setMinsSecs `cat /tmp/stopwatch.turnon`
            sinceStart=`echo $hrs:$mins:$secs`
            setMinsSecs `cat /tmp/stopwatch.10pm`
            till10=`echo $hrs:$mins:$secs`
            if [ ! -f /tmp/stopwatch.start ];
            then
                printf "notRunning $sinceStart $till10"
                exit
            fi
            setMinsSecs `cat /tmp/stopwatch.interrupt`
            echo "$mins minutes $secs seconds"
            ;;
    esac
fi
