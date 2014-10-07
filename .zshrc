# custom imported from my zshrc
PROMPT="%{$fg[red]%}%n%{$reset_color%}@%{$fg[blue]%}%m %{$fg_no_bold[yellow]%}%~%{$reset_color%} $"$'\n'
alias lh='ls -ld .??*'              # only hidden
alias ll='ls -lht'                  # reverse time sorted
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
alias vrc='vim ~/.vimrc'
alias brc='vim ~/.bashrc'
alias zrc='vim ~/.zshrc'
alias gl="git log --pretty=format:'%C(yellow)%h%C(red)%d %C(cyan)%an%Creset %s %Cgreen(%cr)' --graph --all"
alias gs='git status -uall'
alias naughty='find . -type f -exec stat --printf "%x %n\n" "{}" \+ | awk -F"[-: ]" "{print \$1\$2\$3\$4\$5\" \"\$8}" | sort -nr'
function apt-list-packages {
  dpkg-query -W --showformat='${Installed-Size} ${Package} ${Status}\n' | grep -v deinstall | sort -n | awk '{print $1" "$2}'
  }
