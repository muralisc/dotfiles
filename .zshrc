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
# match vim cmdline behavior
# C-f is used for going forward; since very rarely used bind to c-x-e used more frequnetly
bindkey "^F" edit-command-line

# load aliases
[ -f "$HOME/bin/shrc" ] && source "$HOME/bin/shrc"
# source local file
[ ! -f "$HOME/.shrc.local" ] || source "$HOME/.shrc.local"
