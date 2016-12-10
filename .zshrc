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

# initialize fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# initililize fasd
eval "$(fasd --init auto)"
# C-x C-a to do fasd-complete (files and directories)
# or do d,ton<TAb> insted of below
# f,rc.lua<TAB> for files
bindkey '^X^A' fasd-complete
# http://chneukirchen.org/blog/archive/2013/03/10-fresh-zsh-tricks-you-may-not-know.html
bindkey '^[' vi-cmd-mode   # even in default c-x c-v wil go to vim mode
# second one from above link
autoload -Uz copy-earlier-word
zle -N copy-earlier-word
bindkey "^[m" copy-earlier-word
source ~/bin/aliases.sh
# for ruby and nvm( nodejs ) and go
[ -f /usr/share/nvm/init-nvm.sh ] && source /usr/share/nvm/init-nvm.sh
[ ! -f "$HOME/.zshrc.local" ] || source "$HOME/.zshrc.local"

tmux list-sessions 2> /dev/null
# create a tmux for the first teminal spawned
if [ `tmux list-sessions 2> /dev/null | wc -l` -eq 0 ] ; then
# if no tmux session in the machine , create  one and attach
    tmux new-session -s "scratch" -d;
    tmux split-window -v ;
    tmux select-pane -t 1 ;
    tmux new-session -s "Main" -d ;
    tmux attach;
fi
