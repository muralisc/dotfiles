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
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
xcompmgr &
# udiskie &
urxvtd -q -o -f &
dropboxd &
/usr/bin/start-pulseaudio-x11 &
setxkbmap -option caps:none
exec awesome
