#!/bin/bash

clone_if_required() {
  if ! git remote show origin | grep "dotfiles" > /dev/null ; then
    git clone https://github.com/muralisc/dotfiles
    cd dotfiles
  fi
  DOTFILES_PATH=$(pwd)
}

vim_setup() {
  if [[ ! -a ~/.vim/autoload/plug.vim ]] ;
  then
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    mkdir -p ~/.vim/vimundo
  fi
  vim +PlugInstall +qall!   #vim -c PlugInstall -c qall!
}

git_setup() {
  git config --global credential.helper 'cache --timeout=80000'
  git config --global diff.tool 'meld'
  git config --global user.useConfigOnly true
  git config --global core.pager 'less -RS'
  git config --global blame.date relative
  git config --global init.templatedir '~/.git_template'
}

get_zsh_plugins() {
  curl https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/plugins/shrink-path/shrink-path.plugin.zsh \
    --create-dirs -o ~/.local/shrink-path.plugin.zsh
  mkdir -p ~/.zsh; 
  git clone https://github.com/zsh-users/zsh-history-substring-search ~/.zsh/zsh-history-substring-search
}

symlink_files() {
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
}

extra_install() {
  [[ -f ~/.extra ]] && source ~/.extra
}

clone_if_required
git_setup
get_zsh_plugins
symlink_files
vim_setup
extra_install
