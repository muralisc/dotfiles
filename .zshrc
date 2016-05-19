#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# vim mode
bindkey -v
# initialize fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# initililize fasd
eval "$(fasd --init auto)"
# C-x C-a to do fasd-complete (files and directories)
# or do d,ton<TAb> insted of below
# f,rc.lua<TAB> for files
bindkey '^X^A' fasd-complete

source ~/.aliases.sh

# [ -f ~/.localaliases.sh ] && source ~/.localaliases.sh

