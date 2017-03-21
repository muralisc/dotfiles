# enclosing in \[ \] else bash will count these as characters and wrapping wont
# be proper
txtblk='\[\e[0;30m\]' # Black - Regular
txtred='\[\e[0;31m\]' # Red
txtgrn='\[\e[0;32m\]' # Green
txtylw='\[\e[0;33m\]' # Yellow
txtblu='\[\e[0;34m\]' # Blue
txtpur='\[\e[0;35m\]' # Purple
txtcyn='\[\e[0;36m\]' # Cyan
txtwht='\[\e[0;37m\]' # White
bldblk='\[\e[1;30m\]' # Black - Bold
bldred='\[\e[1;31m\]' # Red
bldgrn='\[\e[1;32m\]' # Green
bldylw='\[\e[1;33m\]' # Yellow
bldblu='\[\e[1;34m\]' # Blue
bldpur='\[\e[1;35m\]' # Purple
bldcyn='\[\e[1;36m\]' # Cyan
bldwht='\[\e[1;37m\]' # White
unkblk='\[\e[4;30m\]' # Black - Underline
undred='\[\e[4;31m\]' # Red
undgrn='\[\e[4;32m\]' # Green
undylw='\[\e[4;33m\]' # Yellow
undblu='\[\e[4;34m\]' # Blue
undpur='\[\e[4;35m\]' # Purple
undcyn='\[\e[4;36m\]' # Cyan
undwht='\[\e[4;37m\]' # White
txtrst='\[\e[0m\]'    # Text Reset

export HISTCONTROL=ignoredups:erasedups
export HISTSIZE=10000
export HISTFILESIZE=10000
shopt -s histappend
# to share history among multiple open terminals
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

username="$bldgrn\u"
hostname="$txtylw\h"
filepath="$txtblu\w"
datecolr="$bldpur\D{%F %T}"
#PS1='\u@\h:\w\$ '
PS1="$filepath $txtred❯$txtylw❯$txtgrn❯ $txtrst"

# enable programmable completion features
if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# move here from inputrc
# ----------------------
# match vim cmdline behavior
# C-f is used for going forward; since very rarely used bind to c-x-e used more frequnetly
bind "C-f":edit-and-execute-command
bind 'set completion-ignore-case on'
bind 'set editing-mode vi'
bind 'set keymap vi-insert'             #################keymap vi insert#################
bind '"\e[A":history-substring-search-backward'
bind '"\e[B":history-substring-search-forward'
bind 'TAB: menu-complete'
bind 'set show-mode-in-prompt on'
bind 'set show-all-if-ambiguous on'
bind 'set vi-ins-mode-string \1\e[35m\2+\1\e[0m\2'
bind 'set vi-cmd-mode-string \1\e[33m\2:\1\e[0m\2'
bind '"\C-e": glob-expand-word'
# load aliases
[ -f "$HOME/bin/shrc" ] && source "$HOME/bin/shrc"
# source local file
[ ! -f "$HOME/.shrc.local" ] || source "$HOME/.shrc.local"
