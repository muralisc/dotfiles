#!/bin/bash

# Enahnnce using this http://johnmackintosh.com/2016-12-01-the-hourly-heatmap/
# https://gist.github.com/johnmackintosh/520643a1f82a0c7df00cf949ba98a4e9

# sample cron entry
#   */5 * *   *   *     /home/pi/bin/power-script.sh

unix_epoch=$(date +%s)
log_file=/var/tmp/powerup.log
entries_json=/var/tmp/powerdown.json
touch $log_file $entries_json
last_entry=$(tail -n1 $log_file | awk '{print $1}')

if [[ ! -z $last_entry ]]; then
  threshold_min=31
  threshold_sec=$((threshold_min*60))
  elapsed_time=$((unix_epoch-last_entry))
  if [[ $elapsed_time -gt $threshold_sec ]] ; then
    # put in the down entries if executed after threshold_sec
    echo "
    {
       \"powerdown\": $((last_entry+1)),
       \"powerup\": $((unix_epoch-1))
    }" >> $entries_json
  fi
fi
tail -n 5 $log_file > ${log_file}.tmp
mv ${log_file}.tmp $log_file
echo "$unix_epoch 1 $(date | sed 's/ /_/g')" >> $log_file

