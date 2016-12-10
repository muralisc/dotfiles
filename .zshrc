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

# http://chneukirchen.org/blog/archive/2013/03/10-fresh-zsh-tricks-you-may-not-know.html
bindkey '^[' vi-cmd-mode   # even in default c-x c-v wil go to vim mode
# second one from above link
autoload -Uz copy-earlier-word
zle -N copy-earlier-word
bindkey "^[m" copy-earlier-word

# load aliases
source ~/bin/aliases.sh
# source local file
[ ! -f "$HOME/.zshrc.local" ] || source "$HOME/.zshrc.local"
