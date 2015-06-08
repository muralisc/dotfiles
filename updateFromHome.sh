#!/bin/bash

# copy files|dir in home directory
for i in    \
.aliases.sh \
.bashrc     \
.bcrc       \
.inputrc    \
.vimrc      \
.Xresources \
.zshrc      \
.xinitrc    \
.muttrc     \
.fehbg      \
.gdbinit    \
.Xmodmap    \
.tmux.conf  \
.ssh/config \
.config/inkscape/keys/default.xml   \
.config/user-dirs.dirs              \
;
do
    cp ~/$i ./$i
done

# copy directories
for i in    \
.mutt       \
bin         \
;
do
    cp -r ~/$i ./
done


cp -r ~/.config/awesome ./.config/
cp -r ~/.config/zathura ./.config/
cp -r ~/.config/ranger ./.config/
