while true ; do
  image_count=$(ls ~/shared_folders/transfer_london_home/footage_copy/photos/*{jpg,JPG} | wc -l)
  default_delay=5
  currenttime=$(date +%H:%M)
  if [[ "$currenttime" > "22:00" ]] || [[ "$currenttime" < "06:30" ]]; then
	  echo "$(date) Sleeping for 1 hour during night..."
	  sleep $((1*60*60))
  else
	  feh \
	    --slideshow-delay $default_delay \
	    --auto-rotate \
	    -F \
	    --scale-down \
	    ~/shared_folders/transfer_london_home/footage_copy/photos/*{jpg,JPG} &
	  echo "$(date): Sleeping..."
	  sleep $((default_delay*image_count+default_delay))
	  echo "$(date): Pressing Escape..."
	  xdotool key Escape
	  pkill feh
  fi
done
