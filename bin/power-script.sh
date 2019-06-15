#!/bin/bash

# Enahnnce using this http://johnmackintosh.com/2016-12-01-the-hourly-heatmap/
# https://gist.github.com/johnmackintosh/520643a1f82a0c7df00cf949ba98a4e9

# sample cron entry
#   */10 * *   *   *     /home/pi/bin/power-script.sh

unix_epoch=$(date +%s)
log_file=~/logs/powerup.log
last_entry=$(tail -n1 $log_file | awk '{print $1}')

threshold_sec=700
elapsed_time=$((unix_epoch-last_entry))
if [[ $elapsed_time -gt $threshold_sec ]] ; then
  # put in the down entries if executed after threshold_sec
  echo "$((last_entry+1)) 0" >> $log_file
  echo "$((unix_epoch-1)) 0" >> $log_file
fi
echo "$unix_epoch 1" >> $log_file
