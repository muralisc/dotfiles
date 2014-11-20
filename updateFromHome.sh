#!/bin/bash
cp ~/.aliases.sh ./
cp ~/.bashrc ./
cp ~/.bcrc ./
cp ~/.inputrc ./
cp ~/.vimrc ./
cp ~/.Xresources ./
cp ~/.yaourtrc ./
cp ~/.zshrc ./

mkdir -p .config/awesome
cp -r ~/.config/awesome ./.config/
