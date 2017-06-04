#!/bin/bash

# use this script to place all config files at correct
# location

if [[ ! -d $HOME/dotfiles ]] ;
then
  cd $HOME
  git clone https://github.com/muralisc/dotfiles/
fi

git config --local  user.email "muralisc@gmail.com"
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
fi

curl https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/plugins/shrink-path/shrink-path.plugin.zsh \
  --create-dirs -o ~/.local/shrink-path.plugin.zsh

mkdir -p ~/.zsh; cd ~/.zsh
git clone https://github.com/zsh-users/zsh-history-substring-search
cd ~/

allfiles=$(find $HOME/dotfiles -type f -not \(        \
                          -ipath '*.git/*' -o \
                          -name README.md -o \
                          -name crontab   -o \
                          -name install.sh \) )
for file in $allfiles; do
  homepath=$( sed "s#/dotfiles##" <<< $file )
  mkdir -p $(dirname $homepath)
  ln -vs --backup=numbered $file $homepath
done

vim +PlugInstall +qall!   #vim -c PlugInstall -c qall!
# tmux plugin
if [ ! -d ~/.tmux/plugins/tpm ]; then
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi

# Note to self: this is parameterized in my office dotfiles using env values,
# hence not versioning a .shrc.local in git
if [[ ! -f ~/.shrc.local ]]; then
  socket=`cat /etc/hostname`
  session=`cat /etc/hostname`
echo "
attach_to_tmux $socket $session
alias t='attach_to_tmux $socket $session'
" > ~/.shrc.local
fi
