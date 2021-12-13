# dotfiles

My dotfiles. Works seamlessly for both Mac OS X and Archlinux.

## Installation

### Oneliner
Using curl,
```shell
bash -c "$(curl -fsSL https://raw.githubusercontent.com/muralisc/dotfiles/master/install.sh)"
```

Or using wget
```shell
bash -c "$(wget https://raw.githubusercontent.com/muralisc/dotfiles/master/install.sh -O -)"
```
### Or use Git
```
$ git clone https://github.com/muralisc/dotfiles.git ~/dotfiles
$ cd ~/dotfiles
$ ./install.sh
```

##### Add a `~/.shrc.local`
After install complete, create a `~/.shrc.local` to store the local settings.
For e.g: my `~/.shrc.local` typically contains something like:
```
attach_to_tmux <tmux_socketname> <tmux_session_name>
source /path/to/my/helper/functions/for/work
SONGS_DIRECTORY="path/to/my/songs"
export DOTFILES_MY_ENV_VAR   #nitialize the set of vars used in this repo
some_function_specific_for_this_machine() {
}
```

### Personal git configuration

Create a ~/.extra file to do machine specific steps while installation.
Got this idea from (mathiasbynens/dotfiles)[https://github.com/mathiasbynens/dotfiles]
```
# Git credentials
# Not in the repository, to prevent people from accidentally committing under my name
GIT_AUTHOR_NAME="Murali Suresh"
GIT_AUTHOR_EMAIL="murali@mailinator.com"
GIT_USERNAME="muralisc"
git config --local user.name "$GIT_AUTHOR_NAME"
git config --local user.email "$GIT_AUTHOR_EMAIL"
# set approperiate username in each repo so the libsecret can use it
# more info: https://my-take-on.tech/2019/08/23/safely-storing-git-credentials/
git config --local credential.https://github.com.username "$GIT_USERNAME"
git config --local credential.helper /usr/lib/git-core/git-credential-libsecret
```

Environment variables
Populate in ~/.env.local
```
DOTFILES_ALARM_MEDIA
LEDGER_FILE
DOTFILES_HABBITS
PANE_NAME
```

## Features

* `c` - a command line calculator e.g: `c 2 ^ 3 * 2 ` 
* `decimal_to_binary` - `c` doesn't do this
* `binary_to_decimal` - `c` doesn't do this too
* `attach_to_tmux` - is a helper function which automatically creates a tmux session if no
exist, or attaches to existing one if one is already created.
* `transfer` - Use [https://transfer.sh](https://transfer.sh) to transfer files.
* `ms` - music search for mpd
* `mp` - music play for mpd
* `tt` - time till e.g: `tt tomorrow5am`


## My current Environment
* **Operating System**: [Arch Linux](https://wiki.archlinux.org/index.php/The_Arch_Way)
* **Window Manager**: [awesomewm](https://awesomewm.org/)
* **Terminal**: [rxvt-unicode] + [tmux]
* **Text editor**: [vim](https://www.youtube.com/watch?v=_NUO4JEtkDw)
* **Shell**: [Zsh](https://wiki.archlinux.org/index.php/Zsh)



[rxvt-unicode]: https://wiki.archlinux.org/index.php/Rxvt-unicode
[tmux]: https://tmux.github.io/
