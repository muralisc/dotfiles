# dotfiles

Personal dotfiles for Arch Linux and macOS (dual-platform). Files are **symlinked** into `$HOME` by `install.sh` — not copied. Editing a file here edits it in place for the live system.

## Repo layout

```
bin/          # Scripts and shell config sourced at shell startup
.config/      # XDG config dirs (nvim, kitty, yazi, tmux, etc.)
.zshrc        # Zsh entry point — sources bin/shrc
.bashrc       # Bash entry point — sources bin/shrc
bin/shrc      # Shared shell init: PATH, env vars, sources alias/functions
bin/alias     # All aliases (git, ls, ledger, etc.)
bin/functions # Shell utility functions
crontab       # Cron jobs; auto-saved back here daily at 9:00
install.sh    # Symlink installer
```

## Shell setup

- **Primary shell**: Zsh with vi mode (`jj` → normal, `^E` expands aliases)
- **Autojumper**: `fasd` (`j <dir>` to jump). If fasd is absent, zoxide or autojump can substitute.
- **History**: 100k lines, shared across sessions, substring search via `^P`/`^N`
- **Prompt**: shows PST + IST + local time, vi mode indicator, shrunk path (`~/s/dotfiles` style)
- **FZF**: default command is `fd --type f`, Solarized Dark color scheme

Shell changes go in `bin/alias` or `bin/functions`. Changes to `bin/shrc` affect PATH and env vars shared by both bash and zsh.

## Machine-local overrides (never committed)

| File | Purpose |
|------|---------|
| `~/.shrc.local` | Machine-specific shell init (call `attach_to_tmux`, source work helpers) |
| `~/.env.local` | Secret env vars (`LEDGER_FILE`, `DOTFILES_ALARM_MEDIA`, etc.) |
| `~/.tmux.conf.local` | Machine-specific tmux overrides |
| `~/.extra` | Run during `install.sh` for git credentials and one-time setup |
| `~/bin/private-git-repo-config-helpers` | Private git helpers (sourced by shrc if present) |

Never suggest committing these files.

## Neovim (`config/nvim/init.lua`)

- **Plugin manager**: lazy.nvim (`~/.local/share/nvim/lazy/`), self-bootstraps from `init.lua`
- **LSP**: nvim-lspconfig + none-ls (null-ls fork). Clangd for C/C++, lua_ls for Lua.
- **Completion**: via LSP omnifunc (`<c-x><c-o>`)
- **Fuzzy finder**: Telescope (`<leader>pf` files, `<leader>/` grep, `<leader>ff` same-dir)
- **File manager**: oil.nvim (edit filesystem as buffer)
- **Colorscheme**: gruvbox normally, tokyonight-night in diff mode
- **Formatter**: stylua (via none-ls). Config in `stylua.toml` — 120 cols, 2-space indent.
- **Leader**: Space

After editing `init.lua`, changes take effect on next nvim start or `:source %`.

## Tmux (`.tmux.conf`)

- **Prefix**: `C-a` (primary), `C-Space` (secondary)
- **Pane navigation**: `M-h/j/k/l` (vim-tmux-navigator aware)
- **Window switch**: `M-1..9`
- **Splits**: `C-a \` (vertical), `C-a -` (horizontal), both open in current path
- **Copy mode**: vi keys; `v` to select, `y` to yank → pipes to clipper (port 8377)
- **Extended keys**: enabled (`extended-keys on`) for Claude Code compatibility
- **Passthrough**: enabled for yazi image rendering

## Git workflow

- **Log alias**: `gl` — pretty graph with dates, author, GPG status
- **Diff**: `gdf` uses icdiff side-by-side; `gd` uses meld (dir-diff)
- **Per-repo identity**: use `git_repo_user_set_muralisc` from `bin/git-repo-config-helpers`
- **No global user.email**: `user.useConfigOnly = true` — always set per repo

## Python

Formatter/linter: ruff. Config in `ruff.toml` (nearly all rules enabled). Run `ruff check` and `ruff format` before committing Python files.

## Cron jobs

Defined in `crontab`. The crontab itself is auto-saved back to this file daily at 9:00 AM. To apply changes: `crontab ~/src/dotfiles/crontab`.

## Adding new dotfiles

1. Put the file in the repo at its relative path from `$HOME`
2. Add it to `install.sh`'s `symlink_files` function if needed
3. The installer creates numbered backups (`file.1`, `file.2`) before symlinking

## What NOT to do

- Don't commit secrets, API keys, or anything that belongs in `~/.env.local`
- Don't hardcode machine-specific paths — use `$HOME`, `~`, or the local-override pattern
- Don't add `#!/bin/zsh` shebangs to files in `bin/` that are sourced (not executed) — they use `#!/bin/bash` as a convention but are sourced by both shells
