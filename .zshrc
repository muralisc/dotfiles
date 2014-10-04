autoload -U colors && colors
autoload -U compinit promptinit
compinit
promptinit

#Tab completion should be case-insensitive.
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' special-dirs true
HISTSIZE=10000
SAVEHIST=10000
setopt sharehistory
setopt extendedhistory
setopt hist_ignore_dups
HISTFILE=~/.zsh_history

PROMPT="%{$fg[red]%}%n%{$reset_color%}@%{$fg[blue]%}%m %{$fg_no_bold[yellow]%}%~%{$reset_color%} $"$'\n'

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
alias gl="git log --pretty=format:'%C(yellow)%h%C(red)%d %C(cyan)%an%Creset %s %Cgreen(%cr)' --graph --all"
alias gs='git status -uall'
alias rm='rm -f'
alias naughty='find . -type f -exec stat --printf "%x %n\n" "{}" \+ | awk -F"[-: ]" "{print \$1\$2\$3\$4\$5\" \"\$8}" | sort -nr'
function apt-list-packages {
  dpkg-query -W --showformat='${Installed-Size} ${Package} ${Status}\n' | grep -v deinstall | sort -n | awk '{print $1" "$2}'
  }
