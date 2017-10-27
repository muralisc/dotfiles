#!/bin/bash

# use this script to place all config files at correct
# location

# check if repo is cloned or not
if ! git remote show origin | grep "dotfiles" > /dev/null ; then
  git clone https://github.com/muralisc/dotfiles
  cd dotfiles
fi
DOTFILES_PATH=$(pwd)

git config --global user.email "muralisc@gmail.com"
git config --global user.name "Murali Suresh"
git config --global credential.helper 'cache --timeout=80000'
git config --global credential.helper 'store'
git config --global credential.https://github.com.username 'muralisc'
git config --global diff.tool 'meld'
git config --global user.useConfigOnly true
git config --global --unset-all user.email
git config --global core.pager 'less -RS'
git config --global init.templatedir '~/.git_template'

# if not exit plug.vim
if [[ ! -a ~/.vim/autoload/plug.vim ]] ;
then
  curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  mkdir -p ~/.vim/vimundo
fi

curl https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/plugins/shrink-path/shrink-path.plugin.zsh \
  --create-dirs -o ~/.local/shrink-path.plugin.zsh

mkdir -p ~/.zsh; cd ~/.zsh
git clone https://github.com/zsh-users/zsh-history-substring-search
cd ~/

allfiles=$(find $DOTFILES_PATH -type f -not \( \
                          -ipath '*.git/*' -o  \
                          -name README.md -o   \
                          -name crontab   -o   \
                          -name install.sh \) )
for file in $allfiles; do
  homepath="${HOME}$( sed "s#$DOTFILES_PATH##" <<< $file )"
  mkdir -p $(dirname $homepath)
  ln -vs --backup=numbered $file $homepath
done

vim +PlugInstall +qall!   #vim -c PlugInstall -c qall!
# tmux plugin
if [ ! -d ~/.tmux/plugins/tpm ]; then
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi
