# Fix for strange issue
# Tmux alwasy reads /etc/profile
# and this changes the order of PATH in mac
# So Clear the path vaiable before running /etc/profile
if [ -f /etc/profile ]; then
    PATH=""
    source /etc/profile
fi
if [[ -z "$TMUX" ]] && [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
  exec startx
fi
