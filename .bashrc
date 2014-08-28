# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

export HISTCONTROL=ignoredups:erasedups
export HISTSIZE=10000
export HISTFILESIZE=10000
shopt -s histappend
# to share history among multiple open terminals
export PROMPT_COMMAND="history -a; history -n; $PROMPT_COMMAND"


# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize
# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac
# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes
if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

txtblk='\e[0;30m' # Black - Regular
txtred='\e[0;31m' # Red
txtgrn='\e[0;32m' # Green
txtylw='\e[0;33m' # Yellow
txtblu='\e[0;34m' # Blue
txtpur='\e[0;35m' # Purple
txtcyn='\e[0;36m' # Cyan
txtwht='\e[0;37m' # White
bldblk='\e[1;30m' # Black - Bold
bldred='\e[1;31m' # Red
bldgrn='\e[1;32m' # Green
bldylw='\e[1;33m' # Yellow
bldblu='\e[1;34m' # Blue
bldpur='\e[1;35m' # Purple
bldcyn='\e[1;36m' # Cyan
bldwht='\e[1;37m' # White
unkblk='\e[4;30m' # Black - Underline
undred='\e[4;31m' # Red
undgrn='\e[4;32m' # Green
undylw='\e[4;33m' # Yellow
undblu='\e[4;34m' # Blue
undpur='\e[4;35m' # Purple
undcyn='\e[4;36m' # Cyan
undwht='\e[4;37m' # White
txtrst='\e[0m'    # Text Reset

username="$bldgrn\u"
hostname="$txtblu\h"
filepath="$bldylw\w"
datecolr="$bldpur\D{%F %T}"
if [ "$color_prompt" = yes ]; then
    PS1="$username $hostname $filepath $datecolr \$$txtrst\n"
else
    PS1='\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi
START=`date +%s`
function elapsed_time_since_bash_start {
    END=`date +%s`
    seconds=$(( ($END - $START) ))
    minutes=$(($seconds / 60))
    hours=$(($minutes / 60))
    days=$(($hours / 24))
    if [[ "$days" -ne 0 ]]; then
        echo "$days days "
    fi
    if [[ "$hours" -ne 0 ]]; then
        echo "$(($hours%24)) hours and "
    fi
    if [[ "$minutes" -ne 0 ]]; then
        echo "$(($minutes%60)) minutes and "
    fi
    echo "$(($seconds % 60)) seconds elapsed"
}
#usage : time_till tomorrow5am
function time_till {
    printf "printing time till `date -d $1`  \n\n"
    seconds=$((`date -d $1 +'%s'` - `date +'%s'`))
    minutes=$(($seconds / 60))
    hours=$(($minutes / 60))
    days=$(($hours / 24))
    if [[ "$days" -ne 0 ]]; then
        echo "$days days "
    fi
    if [[ "$hours" -ne 0 ]]; then
        echo "$(($hours%24)) hours and "
    fi
    if [[ "$minutes" -ne 0 ]]; then
        echo "$(($minutes%60)) minutes and "
    fi
    echo "$(($seconds % 60)) seconds elapsed"
}

#show hidden files alone
alias lsh='ls -ld .??*'
#show long listing
alias ll='ls -lht'
alias xdo='xdg-open'
alias du='du -h'
alias install='sudo apt-get install'
alias aptsrh='sudo apt-cache search'

alias dev='cd ~/myfiles/carefull/PersonalData_max26gb/Videos/notForKids/dd'
alias dbms='cd /home/mur/myfiles/carefull/Dropbox/IITB/dbms_cs631'
alias gfx='cd /home/mur/myfiles/carefull/Dropbox/IITB/gfx__cs675'
alias lab='cd /home/mur/myfiles/carefull/Dropbox/IITB/lab__cs699'
alias nlp='cd /home/mur/myfiles/carefull/Dropbox/IITB/NLP__cs626'
alias nwks='cd /home/mur/myfiles/carefull/Dropbox/IITB/nwks_cs641/assignments/pa2/ns-allinone-3.20/ns-3.20'
alias cheat='cd ~/myfiles/carefull/Dropbox/Notes/frequent/cheatsheets/'
alias bundle='cd cd ~/.vim/bundle/'
alias vrc='vim ~/.vimrc'
alias brc='vim ~/.bashrc'
alias gl="git log --pretty=format:'%C(yellow)%h%C(red)%d %C(cyan)%an%Creset %s %Cgreen(%cr)' --graph --all"
alias gs='git status -uall'

source ~/.fzf.bash
