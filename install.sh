#!/bin/bash

# use this script to place all config files at correct
# location

# if not exit plug.vim
if [[ ! -a ~/.vim/autoload/plug.vim ]] ;
then
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

cd ~/dotfiles
if (( "$?"!=0 ));
then
    echo "folder not found"
    exit
fi

mkdir -p $HOME/.config/beets/
mkdir -p $HOME/.config/inkscape/keys
mkdir -p $HOME/.config/vlc
mkdir -p $HOME/.local/share/applications
mkdir -p $HOME/.mpd
mkdir -p $HOME/.ncmpcpp
mkdir -p $HOME/.ssh
mkdir -p $HOME/.vim
mkdir -p $HOME/.vim/vimundo
mkdir -p $HOME/.rtorrentSession
BKPfolder=bak`date +'%Fat%H-%M-%S'`
mkdir -p $HOME/$BKPfolder


for i in                          \
.bashrc                           \
.bcrc                             \
bin                               \
.config/awesome                   \
.config/beets/config.yaml         \
.config/inkscape/keys/default.xml \
.config/mimeapps.list             \
.config/ranger                    \
.config/mpv                       \
.config/user-dirs.dirs            \
.config/vlc/vlcrc                 \
.config/zathura                   \
.fehbg                            \
.gdbinit                          \
.gitconfig                        \
.inputrc                          \
.local/share/applications         \
.mpd/mpd.conf                     \
.muttrc                           \
.ncmpcpp/config                   \
.ssh/config                       \
.tmux.conf                        \
.vimrc                            \
.xinitrc                          \
.Xmodmap                          \
.Xresources                       \
.zshrc                            \
.yaourtrc                         \
.rtorrent.rc                      \
    ;
do
    echo "$HOME/$i making"
    # back up files if not links else delete
    if [ ! -L $HOME/$i ]; then
        mv $HOME/$i $HOME/$BKPfolder/${i//\//-}.bak
    else
        rm $HOME/$i
    fi
    # link files
    ln -s `pwd`/$i $HOME/$i
done
vim +PlugInstall +qall!   #vim -c PlugInstall -c qall!

# delete backup folder if no files
if [ `ls $HOME/$BKPfolder | wc -l` -eq "0" ]; then
    rm $HOME/$BKPfolder
fi
