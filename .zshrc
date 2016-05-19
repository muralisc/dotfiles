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

# Customize to your needs...
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# initililize fasd
eval "$(fasd --init auto)"
# C-x C-a to do fasd-complete (files and directories)
# or do d,ton<TAb> insted of below
bindkey '^X^A' fasd-complete    

source ~/.aliases.sh

# [ -f ~/.localaliases.sh ] && source ~/.localaliases.sh

# vim mode {{{
bindkey -v
function zle-line-init zle-keymap-select {
    RPS1="${${KEYMAP/vicmd/-- NORMAL --}/(main|viins)/}"
    RPS2=$RPS1
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select
# }}}
