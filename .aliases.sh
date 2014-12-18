alias vrc='vim ~/.vimrc'
alias arc='vim ~/.config/awesome/rc.lua'
alias vch='vim ~/Dropbox/IITB/cheatsheets/vim'
alias tlc='vim ~/Dropbox/IITB/cheatsheets/theLinuxCommandline.txt'
alias brc='vim ~/.bashrc'
alias zrc='vim ~/.zshrc'

alias n='urxvt &; disown'
alias l='ls -lFh'       # long, classify , human readable
alias r='ranger'
alias xo='xdg-open'
alias gl="git log --pretty=format:'%C(yellow)%h%C(red)%d %C(cyan)%an%Creset %s %Cgreen(%cr)' --graph --all"
alias gs='git status -uall'
alias ft='find . -ipath "*.git" -prune -o -print| sed -e "s/[^-][^\/]*\//  |/g"'         # no git file tree  --more fileterd
alias dt='find . -ipath "*.git" -prune -o -type d -print| sed -e "s/[^-][^\/]*\//  |/g"'                          # no file (Dir) tree --most filtered
alias pro='sudo pacman -Rns $(pacman -Qtdq)'  # pac remove orphans
alias feh='feh -r --keep-zoom-vp --info "exiv2 %f"'



alias naughty='find . -type f -exec stat --printf "%x %n\n" "{}" \+ | awk -F"[-: ]" "{print \$1\$2\$3\$4\$5\" \"\$8}" | sort -nr'
alias rm='rm -rfv'

EDITOR=/usr/bin/vim
 
export BC_ENV_ARGS=~/.bcrc

function ms {
xmms2 search "*$1*"
}
function mp {
xmms2 search "*$1*"
xmms2 clear ;
xmms2 add -t "*$1*"
xmms2 stop ;
xmms2 play ;
}
function apt-list-packages {
  dpkg-query -W --showformat='${Installed-Size} ${Package} ${Status}\n' | grep -v deinstall | sort -n | awk '{print $1" "$2}'
}


#usage : tt tomorrow5am
# time till
function tt {
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
# simple calculator
function = {
    python -c "print($1)"
    awk "BEGIN { print $* }"
}
