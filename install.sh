#!/bin/bash

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
BKPfolder=bak`date +'%Fat%H-%M-%S'`
mkdir -p $HOME/$BKPfolder


for i in                          \
.aliases.sh                       \
.bashrc                           \
.bcrc                             \
bin                               \
.config/awesome                   \
.config/beets/config.yaml         \
.config/inkscape/keys/default.xml \
.config/mimeapps.list             \
.config/ranger                    \
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
    ;
do
    echo "$HOME/$i making"
    # back up files
    mv $HOME/$i $HOME/$BKPfolder/${i//\//-}.bak
    # link files
    ln -s `pwd`/$i $HOME/$i
done

# install perls for urxvt {{{
# git clone https://github.com/muennich/urxvt-perls
# sudo mv urxvt-perls/* /usr/lib/urxvt/perl/
# git clone https://github.com/majutsushi/urxvt-font-size
# sudo mv urxvt-font-size/* /usr/lib/urxvt/perl/
# rm -rf urxvt-perls
# rm -rf urxvt-font-size
# if  grep -qi 'Arch' /etc/lsb-release ;
# then
#     yaourt -S urxvt-vtwheel
# fi
# }}}
