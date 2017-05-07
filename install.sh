#!/bin/bash

# use this script to place all config files at correct
# location

if [[ ! -d $HOME/dotfiles ]] ;
then
  cd $HOME
  git clone https://github.com/muralisc/dotfiles/
fi

git config --global user.email "muralisc@gmail.com"
git config --global user.name "Murali Suresh"
git config --global credential.helper 'cache --timeout=80000'
git config --global credential.helper 'store'
git config --global credential.username 'muralisc'
git config --global diff.tool 'meld'

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
mkdir -p $HOME/.mpdcron/hooks
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
.local/share/applications         \
.mpd/mpd.conf                     \
.mpdcron/hooks/player             \
.muttrc                           \
.ncmpcpp/config                   \
.ssh/config                       \
.tmux.conf                        \
.vimrc                            \
.xinitrc                          \
.Xmodmap                          \
.bash_profile                     \
.Xresources                       \
.yaourtrc                         \
.rtorrent.rc                      \
    ;
do
  # back up files if not links else delete
  if [ ! -L $HOME/$i ]; then
    mv $HOME/$i $HOME/$BKPfolder/${i//\//-}.bak &> /dev/null
  else
    rm $HOME/$i
  fi
  # link files
  ln -s `pwd`/$i $HOME/$i
done
vim +PlugInstall +qall!   #vim -c PlugInstall -c qall!

# delete backup folder if no files
if [ `ls -la $HOME/$BKPfolder | wc -l` -eq "0" ]; then
  rm -r $HOME/$BKPfolder
fi
# tmux plugin
if [ ! -d ~/.tmux/plugins/tpm ]; then
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi

# Note to self: this is parameterized in my office dotfiles using env values,
# hence not versioning a .shrc.local in git
socket=`cat /etc/hostname`
session=`cat /etc/hostname`
echo "
attach_to_tmux $socket $session
alias t='attach_to_tmux $socket $session'
" > ~/.shrc.local
