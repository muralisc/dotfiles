#!/bin/bash

# echo "save installed files from pacman (rxvt-unicode-256)"
pacman -Q   > pacQ #all packages
pacman -Qe  > pacQe # explicitly installed packages
pacman -Qeq > explicitly_installed_packages.reinstall
pacman -Qm  > possibleFromAur # possibly from aur
# echo "save thunderbird folder"

# echo "save deleted vim colorschemes"
# echo "save .direcories to HD"
