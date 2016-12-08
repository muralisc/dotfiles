#!/bin/bash

# use fasd insted of vim document aliases
alias acs='apt-cache search'
alias agi='sudo apt-get install'
alias c='noglob c'
alias feh='feh -r --info "exiv2 %f" --auto-zoom --geometry 1280x960+320+60 -C /usr/share/fonts/TTF -e Ubuntu-R/35'
alias ft='find . -ipath "*.git" -prune -o -print| sed -e "s/[^-][^\/]*\//  |/g"'         # no git file tree  --more fileterd
# git aliases  {{{
alias ga='git add'
alias gb='git branch'
alias gcam='git commit -am'
alias gcm='git commit -m'
alias gco='git checkout'
alias gdc='git diff --cached'
alias gd='git diff'
alias gf='git fetch origin'
alias gl="git log --pretty=format:'%C(yellow)%h%C(red)%d %C(blue)%an%Creset %s %Cgreen(%cr)' --graph --all"
alias gp='git push origin master'
alias gpl='git pull --rebase origin master'
alias grhh='git reset HEAD --hard'
alias gs='git status -sb'
alias gS='git status -uall'
alias gsp='git stash pop'
alias gst='git stash'
# }}}
alias j='fasd_cd -d'
alias l='ls -1Fh'       # classify , human readable, use ll for long
alias n='ncmpcpp'
alias pro='sudo pacman -Rns $(pacman -Qtdq)'  # pac remove orphans
alias r='ranger'
alias ta='tmux attach'
alias tl='tmux list-sessions'
alias ts='tmuxinator start test'
alias t='tmux new-session -s "scratch" -d; tmux split-window -v ;tmux split-window -v ;tmux new-session -s "Main" -d ; tmux attach;'
alias v='fasd -f -e vim'
alias xo='xdg-open'
alias ync='yaourt --noconfirm'
alias ys='yaourt --color --pager -Ss'
alias y='yaourt'
alias naughty='find . -type f -exec stat --printf "%x %n\n" "{}" \+ | awk -F"[-: ]" "{print \$1\$2\$3\$4\$5\" \"\$8}" | sort -nr'
alias rm=rm
alias rr='rm -rv'
alias rf='rm -rfv'

VISUAL=/usr/bin/vim
EDITOR=/usr/bin/vim

export BC_ENV_ARGS=~/.bcrc

#   functions {{{
# eg ms year:2000 path::Hindi
# eg ms year:..1995 path::Hindi
function ms { #mpc search
    # mpc search filename "`echo $*| sed 's/ /_/g'`"
    beet ls -f '$path' "$@" | sed 's#/home/murali/Dropbox/Songs/##' 
}
function mp { #mpc play
    mpc clear
    # mpc search filename "`echo $*| sed 's/ /_/g'`" | mpc add
    beet ls -f '$path' "$@" | sed 's#/home/murali/Dropbox/Songs/##' | mpc add
    mpc play
}

# database update
function mu {
    rm ~/Dropbox/Songs/musiclibrary.blb; beet import -A ~/Dropbox/Songs/ ;
}

function apt-list-packages {
    dpkg-query -W --showformat='${Installed-Size} ${Package} ${Status}\n' | grep -v deinstall | sort -n | awk '{print $1" "$2}'
}

# time till
function tt {

    if [ $# -eq 0 ];
    then
        echo usage : tt tomorrow5am
        echo usage : tt tomorrow5am -s
        echo usage : sleep `tt 2:37am -c`
        return 1;
    fi

    seconds=$((`date -d $1 +'%s'` - `date +'%s'`))
    minutes=$(($seconds / 60))
    hours=$(($minutes / 60))
    days=$(($hours / 24))

    if [[ $2 = "-c" ]]; then
        # for piping to sleep
        echo "$seconds"
    elif [[ $2 = "-s" ]]; then
        echo "$days days $(($hours%24)):$(($minutes%60)):$(($seconds%60))"
    else
        printf "printing time till `date -d $1`  \n\n"
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
    fi
}
# simple calculator
function c {
    awk "BEGIN { print $1 }"
}

unalias o       # required as zpresto has an alias
function o(){
    nohup xdg-open $* &
}

fucntion transfer() {
    if [ $# -eq 0 ];
    then
        echo "No arguments specified. Usage:\necho transfer /tmp/test.md\ncat /tmp/test.md | transfer test.md";
        return 1;
    fi
    tmpfile=$( mktemp -t transferXXX );
    if tty -s; then
        basefile=$(basename "$1" | sed -e 's/[^a-zA-Z0-9._-]/-/g');
        curl --progress-bar --upload-file "$1" "https://transfer.sh/$basefile" >> $tmpfile;
    else
        curl --progress-bar --upload-file "-" "https://transfer.sh/$1" >> $tmpfile ;
    fi;
    cat $tmpfile;
    rm -f $tmpfile;
}

function cdown(){
    if [ $# -eq 0 ]; 
    then
        echo no arg specified
        echo "funciton <time in min> <beep freq in sec>"
        return 1
    fi
    date1=$((`date +%s` + $1*60));
    elapsed=0
    bells=0
    while [ "$date1" -ge `date +%s` ]; do
        echo -ne "$(date -u --date @$(($date1 - `date +%s`)) +%H:%M:%S)\b\b\b\b\b\b\b\b";
        sleep 1
        elapsed=$(($elapsed + 1))
        if [ "$elapsed" -eq $2 ]; then
            bells=$(($bells + 1))
            for i in `seq 1 $bells`;
            do
                paplay /usr/share/sounds/freedesktop/stereo/bell.oga
            done
            elapsed=0
        fi
    done
    paplay /usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga
}

function timeGoogle(){
    sudo date -s "$(wget -S  "http://www.google.com/" 2>&1 | grep -E '^[[:space:]]*[dD]ate:' | sed 's/^[[:space:]]*[dD]ate:[[:space:]]*//' | head -1l | awk '{print $1, $3, $2,  $5 ,"GMT", $4 }' | sed 's/,//')"
}

# }}}

# add songs by year
# mpc clear; beet ls -f '$path' year:..2000 path::Hindi | sed 's#/home/murali/Dropbox/Songs/##' | mpc add; mpc play
