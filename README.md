# dotfiles

## Installation

### Onliner
Using curl,
```shell
sh -c "$(curl -fsSL https://raw.githubusercontent.com/muralisc/dotfiles/master/install.sh)"
```

Or using wget
```shell
sh -c "$(wget https://raw.githubusercontent.com/muralisc/dotfiles/master/install.sh -O -)"
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

some_function_specific_for_this_machine() {
}
```

## Features

* `c` - a command line calculator e.g: `c 2 ^ 3 * 2 ` 
* `attach_to_tmux` - is a helper function which automatically creates a tmux session if no
exist, or attaches to existing one if one is already created.


## My current Environment
* **Operating System**: [Arch Linux](https://wiki.archlinux.org/index.php/The_Arch_Way)
* **Window Manager**: [awesomewm](https://awesomewm.org/)
* **Terminal**: [rxvt-unicode] + [tmux]
* **Text editor**: [vim](https://www.youtube.com/watch?v=_NUO4JEtkDw)
* **Shell**: [Zsh](https://wiki.archlinux.org/index.php/Zsh)



[rxvt-unicode]: https://wiki.archlinux.org/index.php/Rxvt-unicode
[tmux]: https://tmux.github.io/
