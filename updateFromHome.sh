#!/bin/bash
cp ~/.aliases.sh ./
cp ~/.bashrc ./
cp ~/.bcrc ./
cp ~/.inputrc ./
cp ~/.vimrc ./
cp ~/.Xresources ./
cp ~/.zshrc ./
cp ~/.xinitrc ./

mkdir -p .config/awesome
cp -r ~/.config/awesome ./.config/
cp -r ~/.config/zathura ./.config/
