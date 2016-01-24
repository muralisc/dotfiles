#!/bin/bash


# cd ~/dotfiles
if (( "$?"!=0 ));
then
    echo "folder not found"
    exit
fi

mkdir -p ~/.config/inkscape/keys
mkdir -p ~/.ssh
mkdir -p ~/.vim
mkdir ~/.vim/vimundo
mkdir -p ~/.local/share/applications


for i in \
.aliases.sh \
.bashrc     \
.bcrc       \
bin                                 \
.config/inkscape/keys/default.xml   \
.config/user-dirs.dirs              \
.config/awesome                     \
.config/ranger                      \
.config/zathura                     \
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
    mv ~/$i ~/${i}.bak
    ln -s `pwd`/$i ~/$i
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
