# enclosing in \[ \] else bash will count these as characters and wrapping wont
# be proper
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
wrap(){ echo '\['"$1"'\]'; }
txtblk=$(tput setaf 0)  # txtblk='\[\e[0;30m\]' # Black - Regular
txtred=$(tput setaf 1)  # txtred='\[\e[0;31m\]' # Red
txtgrn=$(tput setaf 2)  # txtgrn='\[\e[0;32m\]' # Green
txtylw=$(tput setaf 3)  # txtylw='\[\e[0;33m\]' # Yellow
txtblu=$(tput setaf 4)  # txtblu='\[\e[0;34m\]' # Blue
txtpur=$(tput setaf 5)  # txtpur='\[\e[0;35m\]' # Purple
txtcyn=$(tput setaf 6)  # txtcyn='\[\e[0;36m\]' # Cyan
txtwht=$(tput setaf 7)  # txtwht='\[\e[0;37m\]' # White
txtrst=$(tput sgr0)

export HISTCONTROL=ignoredups:erasedups
export HISTSIZE=10000
export HISTFILESIZE=10000
shopt -s histappend
# to share history among multiple open terminals

# PROMPT
shrink_bash_path () {
  shrinked_prefix=$(sed "s#$HOME#~#" <<< $(dirname $(pwd)) | tr '/' '\n' | cut -c1 | tr '\n' '/')
  export SHRINKED_PWD="$(echo ${shrinked_prefix}$(basename $(pwd)))"
}
export PROMPT_COMMAND="shrink_bash_path; history -a; history -c; history -r; $PROMPT_COMMAND"
username="$(wrap $bldgrn)\u"
hostname="$(wrap $txtylw)\h"
filepath="$(wrap $txtblu)\$SHRINKED_PWD"
datecolr="$(wrap $bldpur)\D{%F %T}"
#PS1='\u@\h:\w\$ '
export PS1="${SSH_CONNECTION:+ssh }$filepath $(wrap $txtgrn)▶ $(wrap $txtrst)"
if [[ $(id -u) == 0 ]]; then PS1="root|$PS1"; fi

# enable programmable completion features
if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# load aliases
[ -f "$HOME/bin/shrc" ] && source "$HOME/bin/shrc"
# source local file
[ ! -f "$HOME/.shrc.local" ] || source "$HOME/.shrc.local"

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
