#!/bin/bash
# followiing the video of sean Bruen : Arch Linux : from post install to Xorg

###############
# Virtual Box #
###############

# for vagrant box with diff key layout sudo loadkeys us
# install virtualbox-guest-utils if in virtual box


sudo sed 's/^#TotalD/TotalD/' -i /etc/pacman.conf
pacman -Syu
# USER CREATION
useradd -m -g users -s /bin/bash murali
#passwd murali
echo " see edit from 2:00 - 2:20 "
pacman -Syu
echo " reboot if necessary "
pacman -S sudo
echo "visudo /etc/sudoers"
echo "dulpicate root ..murali ALL"
# INSTALL BASIC STUFF TMUX VIM ZSH etc
sudo pacman -S fakeroot git jshon wget make pkg-config autoconf automake patch expac zsh tmux gvim
wget http://aur.archlinux.org/cgit/aur.git/snapshot/packer.tar.gz
tar zxvf packer.tar.gz
cd packer && makepkg
sudo pacman -U ~/packer/packer-20150808-1-any.pkg.tar.xz
packer -S yaourt
~/dotfiles/install.sh
# INSTALL BASIC XSERVER WITH AWESOME
sudo pacman -S alsa-utils xorg-server xorg-xinit xorg-server-utils mesa awesome \
xf86-video-vesa xterm rxvt-unicode xsel xclip --noconfirm
sudo yaourt -S vicious light-git urxvt-perls urxvt-resize-font-git urxvt-vtwheel --noconfirm
git clone https://github.com/terceiro/awesome-freedesktop
mv awesome-freedesktop/freedesktop /etc/xdg/awesome/
rm -rf awesome-freedesktop
echo "skip from 9:20 to 10:50 "
echo "skip from 11:00 to 14.38 "
reboot
# check startx working
git config --global user.email "muralisc@gmail.com"
git config --global user.name "Murali S"
echo "  ===========add inifinality============     "
# ADD infinaliyt repo
sudo sh -c 'cat << END >> /etc/pacman.conf
[infinality-bundle]
Server = http://bohoomil.com/repo/\$arch

[infinality-bundle-multilib]
Server = http://bohoomil.com/repo/multilib/\$arch

[infinality-bundle-fonts]
Server = http://bohoomil.com/repo/fonts
END'
sudo pacman-key -r 962DDE58
sudo pacman-key --lsign-key 962DDE58
sudo pacman -S infinality-bundle
sudo pacman -S ttf-ubuntu-font-family-ib
# GUI TERMINAL URXVT SHOULD BE WORKING BY NOW


# INSTALL AND SETUP ZSH {{{
USERNAME=vagrant
cd ~/
zsh -c 'git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"'
zsh -c 'setopt EXTENDED_GLOB
for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
  ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
done'
sed '/e:prompt/ s/sorin/minimal/' ~/.zpreztorc -i
sed "/[']utility/a \'history-substring-search\' \\\\"  ~/.zpreztorc -i
git clone https://github.com/clvv/fasd
cd fasd
sudo make install
cd ~/
rm -rf fasd
sudo chsh -s /bin/zsh $USERNAME
# }}}



sudo pacman -S jre8-openjdk firefox yajl
yaourt -S mt7601u-git   # for external wifi dongle
sudo pacman -S networkmanager
systemctl enable NetworkManager.service
timedatectl set-timezone Asia/Kolkatta
echo "kernel.sysrq=1" >> /etc/sysctl.d/99-sysctl.conf
# zathura for backup // use browser instead; firefox even shows
# comments,
aurvote
zathura-ps
ncdc    #linux commandline dcpp client

# RANGER STUFF
yaourt -S   ranger w3m ffmpeg ffmpegthumbnailer imagemagick mediainfo --noconfirm

# AUDIO and movies STUFF
alsa-utils
submarine subliminal # subtitle stuff
bluez bluez-utils
pavucontrol pacmixer pulsemixer pulseaudio pulseaudio-alsa pulseaudio-bluetooth
mpd mpc ncmpcpp mpv


# DISK stuff and misc must install
yaourt -S --noconfirm ntfs-3g perl-rename jmtpfs sed rsync tar gzip feh gawk gcc gdb gnuplot grep groff

#  APPS WITHOUT  GUI
#  APPS WITH  GUI

bbswitch
bumblebee
dropbox
faenza-icon-theme
atool htop i3lock
inkscape
iw
leafpad
lib32-mesa-libgl
lib32-nvidia-utils
libreoffice-still
lxappearance nvidia
openssh
package-quer
puddletag
rofi-git
thunderbird
tigervnc
xf86-input-synaptics
xf86-video-intel
xorg-xinit
yaourt
youtube-dl
zukwito-themes
unzip
xorg-xprop
xorg-xev
compton-git
cronie
keynav-git
logkeys-git
xorg-xev
xorg-xwininfo
sshfs
kdebase-runtime ( for kdenlive)



# order mirrorlist
sudo cp /etc/pacman.d/mirrorlist  /etc/pacman.d/mirrorlist.bkp
red -i 's/^#Server/Server/' /etc/pacman.d/mirrorlist.bkp
rankmirrors -n 6 /etc/pacman.d/mirrorlist.bkp > temp
