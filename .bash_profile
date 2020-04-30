# Fix for strange issue
# Tmux alwasy reads /etc/profile
# and this changes the order of PATH in mac
# So Clear the path vaiable before running /etc/profile
if [ -f /etc/profile ]; then
    source /etc/profile
fi
source ~/.bashrc
source ~/bin/check_and_startx
