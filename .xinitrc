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

eval $(gnome-keyring-daemon --start)
export SSH_AUTH_SOCK

# x0vncserver -PasswordFile=/home/jaleel/.vnc/passwd &
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
[ ! -f "$HOME/.xinitrc.local" ] || source "$HOME/.xinitrc.local"
[[ -f ~/.xprofile ]] && source ~/.xprofile
session=$1
echo session: $session
case $session in
  awesome) exec awesome ;;
  *) exec bspwm ;;
esac
# exec bspwm

