#!/bin/bash

# echo "save installed files from pacman (rxvt-unicode-256)"
pacman -Qq  > pacQwithNoversion.reinstall             # all packages
pacman -Qe  > pacQeWithVersio.reinstall               # explicitly installed packages
pacman -Qeq > explicitly_installed_packages.reinstall # with no version
pacman -Qm  > possibleFromAu.reinstall                # possibly from aur
# echo "save thunderbird folder"
# echo "save deleted vim colorschemes"
# echo "save .direcories to HD"
