#!/bin/bash

function hard_limit_100 {
    vol=`pactl list sinks | sed '
        /RUNNIN/, $ {
                    s/.*Volume: f.*\/\(.*\)%.*/\1/p;
                    } ;
        d;'
        `
    if (( $vol >= 100 )) 
    then
        for sink in `pactl list short sinks|cut -f 1`; do
        pactl -- set-sink-volume $sink 100%
        done || amixer set Master 100%
        exit 0
    fi
}

case $1 in
    softer)
        for sink in `pactl list short sinks|cut -f 1`; do
        pactl -- set-sink-volume $sink -5%
        done || amixer set Master 5%-
        exit 0
        ;;

    louder)
        for sink in `pactl list short sinks|cut -f 1`; do
        pactl -- set-sink-volume $sink +5%
        done || amixer set Master 5%+
        # hard_limit_100          # funciton call volume should not go above 100
        exit 0
        ;;
    toggle)
        mute_status=`
            pactl list sinks | sed '
            /RUNNIN/, $ {
                        s/Mute: \(.*\)/\1/p;
                        } ;
            d;'
            `
        if [[ $mute_status != *"yes"* ]]; then
            toggleVal=1
        else
            toggleVal=0
        fi
        for sink in `pactl list short sinks|cut -f 1`; do
        pactl -- set-sink-mute $sink $toggleVal
        done || amixer set Master toggle
        exit 0
        ;;
    getVol)
        mute_status=`
            pactl list sinks | sed '
            /RUNNIN/, $ {
                        s/Mute: \(.*\)/\1/p;
                        } ;
            d;'
            `
        if [[ $mute_status != *"yes"* ]]; then
            pactl list sinks | awk '
            /State: [^R]/ {running=0}
            /State: R/ {running=1}
            /Volume: f/   { if ( running == 1) { print $5" "$12; exit} }
            /Volume: f/   { if ( running == 0) { print $5" "$12;} }
            ' | awk '{print $1}'
        else
            echo "----"
        fi
        ;;
    display)
            pactl list sinks | awk '
            /State: [^R]/   {running=0};
            /State: R/      {running=1};
            /State/         { if ( running == 1) { print $0} }
            /Description/   { if ( running == 1) { print $0} }
            /Mute/          { if ( running == 1) { print $0} }
            /Volume: f/     { if ( running == 1) { print $5" "$12} };
            '
            # echo "****************************"
            # amixer sget Master
        ;;
    *)
        echo "Use command line options
    softer
    louder
    toggle
    getVol
    display
        "
        ;;
esac


