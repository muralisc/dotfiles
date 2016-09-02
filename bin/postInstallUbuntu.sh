#!/bin/bash
cd $HOME
sudo add-apt-repository ppa:klaus-vormweg/awesome -y
sudo apt-get update
sudo apt-get upgrade -y
sudo apt install -y awesome xinit git zsh tmux ttf-ubuntu-font-family rxvt-unicode-256color xclip xsel firefox vim-gnome openssh-server rsync
if [ ! -f "$HOME/dotfiles" ]; then
    git clone https://github.com/muralisc/dotfiles
fi
$HOME/dotfiles/install.sh
git clone https://github.com/Mic92/vicious
git clone https://github.com/terceiro/awesome-freedesktop
git clone https://github.com/muennich/urxvt-perls
git clone https://github.com/simmel/urxvt-resize-font
sudo ln -sf /usr/share/zoneinfo/Asia/Calcutta /etc/localtime

if [ ! -d  "${ZDOTDIR:-$HOME}/.zprezto" ]; then
    zsh -c 'git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"'
    zsh -c 'setopt EXTENDED_GLOB
    for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
    ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
    done'

    sed '/e:prompt/ s/sorin/minimal/' $HOME/.zpreztorc -i
    sed "/[']utility/a \'history-substring-search\' \\\\"  $HOME/.zpreztorc -i
    git clone https://github.com/clvv/fasd
    cd fasd
    sudo make install
    cd $HOME/
    rm -rf $HOME/fasd
fi

sudo mv vicious /etc/xdg/awesome/
sudo mv awesome-freedesktop/freedesktop /etc/xdg/awesome/
sudo mv $HOME/urxvt-perls/* /usr/lib/urxvt/perl/
sudo mv urxvt-resize-font/resize-font /usr/lib/urxvt/perl/
sudo rm -rf /usr/share/vim/vim74/colors/* $HOME/urxvt-* $HOME/awesome-freedesktop
cd dotfiles
git update-index --assume-unchanged .config/awesome/brightness.lua .config/awesome/rc.lua .config/awesome/theme.lua
cd $HOME/
sudo chsh -s /bin/zsh $USER
# autostart x-server on login
cat<<END >> $HOME/.zprofile
if [ -z "\$DISPLAY" ] && [ -n "\$XDG_VTNR" ] && [ "\$XDG_VTNR" -eq 1 ]; then
  exec startx
fi
END
echo "done"
#### more stuff
# sudo apt-get install feh git-gui gitk htop
# sudo apt-get install feh vlc youtube-dl zathura zathura-pdf-poppler zsh
# git clone https://github.com/haikarainen/light
