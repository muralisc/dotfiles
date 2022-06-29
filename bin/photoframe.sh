while true ; do
  image_count=$(ls ~/shared_folders/transfer_london_home/footage_copy/photos/*{jpg,JPG} | wc -l)
  default_delay=5
  keypress_interval_sec=25
  currenttime=$(date +%H:%M)
  if [[ "$currenttime" > "22:00" ]] || [[ "$currenttime" < "06:30" ]]; then
	  pkill feh
	  echo "$(date) Sleeping for 1 hour during night..."
	  sleep $((1*60*60))
  elif false ; then # TODO: files changed
          echo "$(date) killing feh as new files are added..."
	  pkill feh
  else
	  if ! pgrep feh ; then
                  echo "$(date) No running instance of feh starting..."
		  feh \
		    --slideshow-delay $default_delay \
		    --auto-rotate \
		    -F \
		    --scale-down \
		    ~/shared_folders/transfer_london_home/footage_copy/photos/*{jpg,JPG} &
	  fi
	  echo "$(date): Sleeping for $keypress_interval_sec sec..."
	  sleep $keypress_interval_sec
	  echo "$(date): Pressing Return to prevent screen off..."
	  xdotool key Return
  fi
done
