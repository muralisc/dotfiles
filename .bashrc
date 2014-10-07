export HISTCONTROL=ignoredups:erasedups
export HISTSIZE=10000
export HISTFILESIZE=10000
shopt -s histappend
# to share history among multiple open terminals
export PROMPT_COMMAND="history -a; history -n; $PROMPT_COMMAND"

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt

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
#PS1='\u@\h:\w\$ '
PS1="$username $hostname $filepath $datecolr \$$txtrst\n"


# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
#if [ -f ~/.bash_aliases ]; then
#    . ~/.bash_aliases
#fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /usr/share/bash-completion/bash_completion ]; then
. /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
. /etc/bash_completion
fi


alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias lsh='ls -ld .??*'
alias ll='ls -lht'
alias xdo='xdg-open'
alias du='du -h'
alias install='sudo apt-get install'
alias aptsrh='sudo apt-cache search'
alias dev='cd ~/myfiles/carefull/playground/experiments/'
alias dbms='cd /home/mur/myfiles/carefull/Dropbox/IITB/dbms_cs631'
alias gfx='cd /home/mur/myfiles/carefull/Dropbox/IITB/gfx__cs675'
alias lab='cd /home/mur/myfiles/carefull/Dropbox/IITB/lab__cs699'
alias nlp='cd /home/mur/myfiles/carefull/Dropbox/IITB/NLP__cs626'
alias nwks='cd /home/mur/myfiles/carefull/Dropbox/IITB/nwks_cs641/assignments/'
alias cheat='cd ~/myfiles/carefull/Dropbox/Notes/frequent/cheatsheets/'
alias bundle='cd cd ~/.vim/bundle/'
alias vrc='vim ~/.vimrc'
alias brc='vim ~/.bashrc'
alias zrc='vim ~/.zshrc'
alias gl="git log --pretty=format:'%C(yellow)%h%C(red)%d %C(cyan)%an%Creset %s %Cgreen(%cr)' --graph --all"
alias gs='git status -uall'
alias rm='rm -f'
alias naughty='find . -type f -exec stat --printf "%x %n\n" "{}" \+ | awk -F"[-: ]" "{print \$1\$2\$3\$4\$5\" \"\$8}" | sort -nr'

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
    echo "$(($seconds % 60)) seconds"
}
