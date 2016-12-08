#!/bin/bash
# script to automatically open zathura in tag 2 in Awesome WM
# xdotool key super+2
xdotool key super+3
xdotool key super+p
sleep 0.1
xdotool type zathura
xdotool key Return
sleep 0.2
# xdotool key super+4
