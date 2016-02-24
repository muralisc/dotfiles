#!/bin/bash
if [[ $1 = "arch" ]] ; then
    # followiing the video of sean Bruen : Arch Linux : from post install to Xorg
    useradd -m -g users -s /bin/bash murali
    #passwd murali
    echo " see edit from 2:00 - 2:20 "
    pacman -Syu
    echo " reboot if necessary "
    pacman -S sudo
    echo "visudo /etc/sudoers"
    echo "dulpicate root ..murali ALL"
    sudo pacman -S fakeroot git jshon wget make pkg-config autoconf automake patch expac
    wget http://aur.archlinux.org/cgit/aur.git/snapshot/packer.tar.gz
    tar zxvf packer.tar.gz
    cd packer && makepkg
    sudo pacman -U ~/packer/packer-20150808-1-any.pkg.tar.xz
    echo "xorg"
    sudo pacman -S alsa-utils
    sudo pacman -S xorg-server xorg-xinit xorg-server-utils mesa
    echo "skip from 9:20 to 10:50 "
    sudo pacman -S xf86-video-vesa
    echo "skip from 11:00 to 14.38 "
    reboot
    echo "  ===========add inifinality============     "
    sudo pacman -S awesome
    sudo pacman -S jre8-openjdk
    sudo pacman -S gvim
    sudo pacman -S rxvt-unicode
    ~/bin/connect_To_iitb "muralis" 'rum!la'
    sudo pacman -Syy
    sudo pacman -S firefox yajl
    # firefox addons
    # Facebook Messenger
    # Html5 Video everywhere
    # ipswitcher
    # LeechBlock
    wget https://aur.archlinux.org/cgit/aur.git/snapshot/yaourt.tar.gz
    wget https://aur.archlinux.org/cgit/aur.git/snapshot/package-query.tar.gz
    makepkg
    sudo pacman -U package-query-1.6.2-1-x86_64.pkg.tar.xz
    makepkg
    sudo pacman -U yaourt-1.6-1-any.pkg.tar.xz
    yaourt -S light-git
    yaourt -S mt7601u-git
    sudo pacman -S networkmanager
    systemctl enable NetworkManager.service
    timedatectl set-timezone Asia/Kolkatta
    echo "kernel.sysrq=1" >> /etc/sysctl.d/99-sysctl.conf
# zathura for backup // use browser instead

echo "INSTALL following ====================" 
ranger w3m # for image preview in ranger 
ntfs-3g alsa-utils bbswitch bluez bluez-utils bumblebee dropbox gnuplot
faenza-icon-theme feh gawk gcc gdb git gnuplot grep groff gvim
atool gzip htop i3lock inkscape iw leafpad lib32-mesa-libgl lib32-nvidia-utils
libreoffice-still light linuxdcpp lxappearance nvidia openssh package-query
pavucontrol perl-rename pulseaudio pulseaudio-alsa jmtpfs imagemagick puddletag
xsel pulseaudio-bluetooth 
rofi-git rsync sed sudo tar texlive-core
thunderbird tigervnc tmux vlc wget
xf86-input-synaptics xf86-video-intel xorg-server xorg-server-utils
xorg-xinit yaourt youtube-dl zsh zukwito-themes xclip yajl perl-rename unzip
xorg-xprop xorg-xev compton-git
cronie
keynav-git
logkeys-git
xorg-xev
xorg-xwininfo

echo "optional:"
texlive-core

git config --global user.email "muralisc@gmail.com"


# git config --global user.name "Murali S"
# install prezto install fasd
else sudo apt-get update sudo apt-get upgrade sudo apt-get install \ feh git
    git-gui gitk htop i3lock inkscape leafpad linuxdcpp openssh-server rsync
    rxvt-unicode-256color texlive-latex-extra texlive-fonts-recomended tmux
    xsel vim-gtk vlc youtube-dl zathura zathura-pdf-poppler zsh
    sudo add-apt-repository ppa:klaus-vormweg/awesome
    sudo apt-get update
    sudo apt-get install awesome
    git clone https://github.com/haikarainen/light
git config --global user.email "muralisc@gmail.com"
git config --global user.name "Murali S"
fi

