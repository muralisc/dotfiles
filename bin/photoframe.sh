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
	  pkill feh
  else
	  if ! pgrep feh ; then
		  feh \
		    --slideshow-delay $default_delay \
		    --auto-rotate \
		    -F \
		    --scale-down \
		    ~/shared_folders/transfer_london_home/footage_copy/photos/*{jpg,JPG} &
	  fi
	  echo "$(date): Sleeping for $keypress_interval_sec sec..."
	  sleep $keypress_interval_sec
	  echo "$(date): Pressing Escape to prevent screen off..."
	  xdotool key Escape
	  pkill feh
	  # mpv --loop-playlist=no --osd-level=3 --fs ~/shared_folders/transfer_london_home/footage_copy/videos/*{mp4,MP4}
  fi
done
