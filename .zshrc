# Set up the prompt


autoload -U colors && colors
PS1="%{$fg_bold[green]%}%n%{$reset_color%}%{$fg_bold[red]%}@%{$reset_color%}%{$fg[blue]%}%m %{$fg_bold[yellow]%}%d %{$reset_color%}
%#"


# ===== History
setopt append_history # Allow multiple terminal sessions to all append to one zsh command history
setopt inc_append_history # Add comamnds as they are typed, don't wait until shell exit
setopt hist_expire_dups_first # when trimming history, lose oldest duplicates first
setopt hist_ignore_dups # Do not write events to history that are duplicates of previous events
setopt hist_ignore_space # remove command line from history list when first character on the line is a space
setopt hist_find_no_dups # When searching history don't display results already cycled through twice
setopt hist_reduce_blanks # Remove extra blanks from each command line being added to history
setopt hist_verify # don't execute, just expand history
setopt share_history # imports new commands and appends typed commands to history
