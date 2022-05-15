while true ; do
  image_cnt=$(ls ~/shared_folders/transfer_london_home/footage_copy/*jpg | wc -l)
  default_delay=3

  feh \
    --action 'printf "%%s\n" %F | xsel -b' \
    --font /usr/share/fonts/TTF/DejaVuSans.ttf/22 \
    --info "exiftool %F  | egrep '(Modification Date|Create Date)'" \
    --slideshow-delay $default_delay -F -. --draw-filename ~/shared_folders/transfer_london_home/footage_copy/*jpg &
  sleep $((default_delay*image_cnt+default_delay))
  pkill feh
  mpv --loop-playlist=no --osd-level=3 --fs ~/shared_folders/transfer_london_home/footage_copy/*{mp4,MP4}
done
