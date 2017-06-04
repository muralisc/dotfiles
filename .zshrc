autoload -Uz compinit
compinit

# prompt setup
setopt PROMPT_SUBST
source ~/.local/shrink-path.plugin.zsh
PS1='%F{blue}$(shrink_path -f) %F{red}❯%F{green}❯%F{blue}❯%f%b '

# vi mode setup
bindkey -v
export KEYTIMEOUT=1
function zle-line-init zle-keymap-select {
    VIM_PROMPT="%F{magenta}[% NORMAL]%"
    RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/}"
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char

# for bringing up editor on C-x C-e
autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

# history
export SAVEHIST=10000
export HISTFILE=~/.zsh_history
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY # Append history without waiting until shell exit
setopt HIST_IGNORE_DUPS   # Check if duplicate of the previous command
setopt HIST_FIND_NO_DUPS  # While searching history, avoid showing duplicates
setopt HIST_IGNORE_SPACE  # Ignore commands starting with a space
setopt HIST_REDUCE_BLANKS # Trim blanks off commands
setopt EXTENDED_HISTORY   # More information in history (timestamp etc.)
source ~/.zsh/zsh-history-substring-search/zsh-history-substring-search.zsh
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# load aliases
[ -f "$HOME/bin/shrc" ] && source "$HOME/bin/shrc"
# source local file
[ ! -f "$HOME/.shrc.local" ] || source "$HOME/.shrc.local"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
