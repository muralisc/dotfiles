#!/bin/bash

echo "save installed files from pacman (rxvt-unicode-256)"
echo "save thunderbird folder"
pacman -Qeq > explicitly_installed_packages.reinstall

echo "save deleted vim colorschemes"
echo "save .direcories to HD"
