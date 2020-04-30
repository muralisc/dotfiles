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

# x0vncserver -PasswordFile=/home/jaleel/.vnc/passwd &
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
[ ! -f "$HOME/.xinitrc.local" ] || source "$HOME/.xinitrc.local"
[[ -f ~/.xprofile ]] && source ~/.xprofile
export EDITOR=vim
udiskie -t &
sxhkd &
mpd &
mpdscribble &
# dropbox &
beet mpdstats &
# caps remapped more comprehensively in Xmodmap
# setxkbmap -option ctrl:nocaps
xmodmap ~/.Xmodmap
keynav daemonize
feh --bg-fill ~/.local/share/wallpaper/wallpaper
# dont turn off the screen soon, wait 2 hours, change every 60sec
xset -dpms
xset s 7200 60
session=$1
echo session: $session
case $session in
  bspwm) exec bspwm ;;
  *) exec awesome ;;
esac
# exec bspwm

