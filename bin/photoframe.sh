# while true ; do
  image_cnt=$(ls ~/shared_folders/transfer_london_home/footage_copy/*jpg | wc -l)
  default_delay=2
  feh --slideshow-delay $default_delay -F -. --draw-filename ~/shared_folders/transfer_london_home/footage_copy/*jpg &
  sleep $((default_delay*image_cnt+default_delay))
  pkill feh
  mpv --osd-level=3 --fs ~/shared_folders/transfer_london_home/footage_copy/*mp4
# done
