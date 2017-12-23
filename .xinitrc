#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice

# x0vncserver -PasswordFile=/home/jaleel/.vnc/passwd &
# [[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
[ ! -f "$HOME/.xinitrc.local" ] || source "$HOME/.xinitrc.local"
export EDITOR=vim
devmon --exec-on-drive "notify-send --icon=block-device --urgency=low \"Volume %l has been mounted\"" &
mpd &
mpdscribble &
dropbox &
beet mpdstats &
#~/thing/syncthing -no-browser &
pulseaudio --start &
# caps remapped more comprehensively in Xmodmap
# setxkbmap -option caps:none
xmodmap ~/.Xmodmap
keynav daemonize
# feh --bg-fill Downloads/wallp_custom_gimp_svg/vim-shortcuts2560x1600_black.png
# dont turn off the screen soon, wait 2 hours, change every 60sec
xset -dpms
xset s 7200 60
# ensure that xrandr recognises HDMI and eDP labels
~/bin/switch-monitor --auto
exec awesome

