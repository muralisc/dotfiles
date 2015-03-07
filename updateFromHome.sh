#!/bin/bash
cp ~/.aliases.sh ./
cp ~/.bashrc ./
cp ~/.bcrc ./
cp ~/.inputrc ./
cp ~/.vimrc ./
cp ~/.Xresources ./
cp ~/.zshrc ./
cp ~/.xinitrc ./
cp ~/.muttrc ./
cp ~/.fehbg ./

mkdir -p .config/
cp -r ~/.config/awesome ./.config/
cp -r ~/.config/zathura ./.config/

cp -r ~/.mutt ./
cp -r ~/bin ./
