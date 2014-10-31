#!/bin/sh -e

# Take a screenshot
# scrot ~/Downloads/wallpaperLock.png

# Pixellate it 10x
# mogrify -scale 10% -scale 1000% ~/Downloads/wallpaperLock.png
# convert -resize 1920 bb9a5897307ea096bd66dd658dfa1d75.jpg wallpaperLock.png
# 1920x1080

# Lock screen displaying this image.
i3lock -i ~/Downloads/wallpaperLock.png

# Turn the screen off after a delay.
sleep 60; pgrep i3lock && xset dpms force off
