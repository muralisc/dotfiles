#!/bin/bash
mv ~/.aliases.sh  ~/.aliases.sh.bak
mv ~/.bashrc      ~/.bashrc.bak
mv ~/.bcrc        ~/.bcrc.bak
mv ~/.inputrc     ~/.inputrc.bak
mv ~/.vimrc       ~/.vimrc.bak
mv ~/.Xresources  ~/.Xresources.bak
mv ~/.zshrc       ~/.zshrc.bak
mv ~/.xinitrc     ~/.xinitrc.bak
mv ~/.muttrc      ~/.muttrc.bak
mv ~/.fehbg       ~/.fehbg.bak



ln -s `pwd`/.aliases.sh ~/.aliases.sh
ln -s `pwd`/.bashrc     ~/.bashrc
ln -s `pwd`/.bcrc       ~/.bcrc
ln -s `pwd`/.inputrc    ~/.inputrc
ln -s `pwd`/.vimrc      ~/.vimrc
ln -s `pwd`/.Xresources ~/.Xresources
ln -s `pwd`/.zshrc      ~/.zshrc
ln -s `pwd`/.xinitrc    ~/.xinitrc
ln -s `pwd`/.muttrc     ~/.muttrc
ln -s `pwd`/.fehbg      ~/.fehbg


git clone https://github.com/muennich/urxvt-perls
sudo mv urxvt-perls/* /usr/lib/urxvt/perl/
git clone https://github.com/majutsushi/urxvt-font-size
sudo mv urxvt-font-size/* /usr/lib/urxvt/perl/
rm -rf urxvt-perls
rm -rf urxvt-font-size

if  grep -qi 'Arch' /etc/lsb-release ;
then
    yaourt -S urxvt-vtwheel
fi
