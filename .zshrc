# Enable custom completions for zsh (e.g rg, see github.com/muralisc/dotfiles/install.sh) {{{
fpath=(~/.zsh/completion $fpath)
autoload -Uz compinit
compinit
# }}}

# for bringing up editor on C-x C-e
autoload -z edit-command-line
# for refreshing vi mode prompt
function zle-line-init zle-keymap-select {
    VIM_PROMPT="%F{magenta}[% N]"
    INS_PROMPT="%F{black}[% I]"
    VIMODE="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/$INS_PROMPT}"
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select
zle -N edit-command-line

expand-aliases() {
  unset 'functions[_expand-aliases]'
  functions[_expand-aliases]=$BUFFER
  (($+functions[_expand-aliases])) &&
    BUFFER=${functions[_expand-aliases]#$'\t'} &&
    CURSOR=$#BUFFER
}

zle -N expand-aliases
bindkey '\e^E' expand-aliases

# allow comments
setopt interactivecomments
# prompt setup
setopt PROMPT_SUBST
if [ -f ~/.local/shrink-path.plugin.zsh ]; then
  source ~/.local/shrink-path.plugin.zsh
fi
at_italics=%{$'\e[3m'%}
at_italicsoff=%{$'\e[23m'%}
at_normal=%{$'\e[0m'%}

ARROWS='%B%F{red}❯%F{green}❯%F{blue}❯%f%b'
DOLLAR=' %B%F{blue}$%f%b'
PST_TIMEZONE="$(TZ=America/Los_Angeles date +%H:%M\ %Z)"
IND_TIMEZONE="$(TZ=Asia/Kolkata date +%H:%M\ %Z)"
LDN_TIMEZONE="$(TZ=Europe/London date +%H:%M\ %Z)"
LCL_TIMEZONE="%D{%H:%M %Z}"
# OTR_TIMEZONE="[${LDN_TIMEZONE}]"
OTR_TIMEZONE=""
TIMEZONES="%F{yellow}${OTR_TIMEZONE}%F{magenta} ${LCL_TIMEZONE} "
# Add color at end for giving color to user input
PS1='${TIMEZONES}${VIMODE} %F{green}%B$(shrink_path -f)%b%f${PANE_NAME}${DOLLAR} %F{white}%B'

# Reset so that command output is set to default colors
preexec () { echo -ne "\e[0m" }
if [[ -n $SSH_CONNECTION ]]; then
    PROMPT="%F{yellow}%n%f@%F{magenta}%m ${PROMPT}"
fi
PS0="test"
# vi mode setup
bindkey -v
bindkey jj vi-cmd-mode #or use ctrl+[
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey "^w" backward-kill-word
bindkey "^X^E" edit-command-line
# Alt+.
bindkey -M viins '\e.' insert-last-word
bindkey '^P' history-substring-search-up #Ctrl-p-up-history
bindkey '^N' history-substring-search-down #Ctrl-n-down-history
bindkey -M vicmd 'k' history-substring-search-up #up-history
bindkey -M vicmd 'j' history-substring-search-down #down-history
# up and down arrows
bindkey '^[[A' history-substring-search-up #up-history
bindkey '^[[B' history-substring-search-down #down-history


# history
export HISTSIZE=10000
export SAVEHIST=10000
export HISTFILE=~/.zsh_history
setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY          # Write the history file in the ':start:elapsed;command' format.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire a duplicate event first when trimming history.
setopt HIST_IGNORE_DUPS          # Do not record an event that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete an old recorded event if a new event is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a previously found event.
setopt HIST_IGNORE_SPACE         # Do not record an event starting with a space.
setopt HIST_SAVE_NO_DUPS         # Do not write a duplicate event to the history file.
setopt HIST_BEEP                 # Beep when accessing non-existent history.
source ~/.zsh/zsh-history-substring-search/zsh-history-substring-search.zsh

# Set title as the last command
# preexec () { print -Pn "\e]0;$1\a" }

[ -f "$HOME/.completions.zsh" ] && source "$HOME/.completions.zsh"

# load aliases
[ -f "$HOME/bin/shrc" ] && source "$HOME/bin/shrc"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# source local file
[ ! -f "$HOME/.shrc.local" ] || source "$HOME/.shrc.local"
