#!/bin/bash


# cd ~/dotfiles
if (( "$?"!=0 ));
then
    echo "folder not found"
    exit
fi

mkdir -p $HOME/.config/inkscape/keys
mkdir -p $HOME/.config/beets/
mkdir -p $HOME/.ssh
mkdir -p $HOME/.mpd
mkdir -p $HOME/.vim
mkdir -p $HOME/.vim/vimundo
mkdir -p $HOME/.local/share/applications


for i in \
.aliases.sh \
.bashrc     \
.bcrc       \
bin                                 \
.local/share/applications           \
.config/inkscape/keys/default.xml   \
.config/user-dirs.dirs              \
.config/awesome                     \
.config/ranger                      \
.config/beets/config.yaml           \
.config/zathura                     \
.config/mimeapps.list               \
.mpd/mpd.conf                       \
.fehbg      \
.gdbinit    \
.inputrc    \
.mutt       \
.muttrc     \
.gitconfig  \
.ssh/config \
.tmux.conf  \
.vimrc      \
.xinitrc    \
.Xmodmap    \
.Xresources \
.zshrc      \
    ;
do
    if [[ -L "$HOME/$i" ]];
    then
        # if symbolic link already exits do nothing
        :   # empty placeholder for later
    else
        echo "$HOME/$i making"
        mv $HOME/$i $HOME/${i}.bak
        ln -s `pwd`/$i $HOME/$i
    fi
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
