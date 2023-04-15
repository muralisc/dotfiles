" vim: foldlevel=2:

" Courtsey:
"   Vincent Driessen <vincent@datafox.nl> 
"       http://nvie.com/posts/how-i-boosted-my-vim/
"   Tsung-Hsiang (Sean) Chang <vgod@vgod.tw> 
"       https://github.com/vgod/vimrc/blob/master/vimrc
"   junegunn: 
"       https://github.com/junegunn/dotfiles/blob/master/vimrc
"   https://github.com/yoshuawuyts/dotfiles
"   And Vim User Manual


" Set as 'not compatible' with the old-fashion vi mode
set nocompatible

"---------------------------------------------------------------------------
" vim-plug setup
"---------------------------------------------------------------------------
" {{{

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif
if filereadable(expand("~/.vim/autoload/plug.vim"))
  call plug#begin('~/.vim/plugged')

  " The plugins are ordered by their names
  " Helper:
  " grep 'Plug ' ~/.vimrc | head -32 | sort -t '/' -k 2

  " asyncrun.vim
  " Usecase:
  "     Run a build command
  "     1. Async in vim
  "     2. Or in toggleterm
  "     3. Or in tmux
  Plug 'skywind3000/asyncrun.vim'
  Plug 'chazy/cscope_maps'
  " diffconflicts - easily address diffconfilicts in nvim :DiffConflicts
  Plug 'whiteinge/diffconflicts'
  Plug 'junegunn/fzf', { 'dir': '~/.fzf'}
  Plug 'junegunn/fzf.vim'
  " leap.nvim - mapped to s in 'normal' mode
  " Usecase:
  "     Jump to a location in visible buffer area - use leap
  "     Jump to a location in any buffer area - use native vim search
  Plug 'ggandor/leap.nvim'
  Plug 'itchyny/lightline.vim'
  Plug 'jose-elias-alvarez/null-ls.nvim'
  Plug 'neovim/nvim-lspconfig'
  " nvim-miniyank block paste fix for nvim
  " Usecase:
  "     Fix for: Block paste not working when clipboard=unnamed
  "     https://github.com/neovim/neovim/issues/1822
  Plug 'bfredl/nvim-miniyank'
  Plug 'nvim-tree/nvim-tree.lua'
  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
  Plug 'nvim-tree/nvim-web-devicons'
  Plug 'nvim-lua/plenary.nvim'
  " tabular - Massively useful plugin for easily aligning text
  Plug 'godlygeek/tabular'
  " toggleterm.nvim (c-t, esc:c-j)
  "     Default Alternatives
  "         :sp term://zsh or 
  "         :vs term://zsh 
  "         ESC -> <c-\><c-n>
  Plug 'akinsho/toggleterm.nvim'
  "Plug 'wincent/vim-clipper'
  " vim-commentary map: gcc
  Plug 'tpope/vim-commentary'
  Plug 'derekwyatt/vim-fswitch'
  Plug 'tpope/vim-fugitive'
  " vim-ledger
  " Provides :LedgerAlign and :LedgerAlignBuffer
  " Better aligned with Tabularize
  " :Tabularize /=/l12c1r0
  Plug 'ledger/vim-ledger'
  " vim-plugin-viewdoc - For viewing help files
  Plug 'powerman/vim-plugin-viewdoc'
  Plug 'airblade/vim-rooter'
  " vim-surround see `:help surround`
  "     cs'"  - change ' around text to "
  "     ysiW) - add ) around word
  Plug 'tpope/vim-surround'
  Plug 'christoomey/vim-tmux-navigator'
  " vim-unimpaired: Awesome bracket maps
  "     [q ]q :cprevious :cnext
  "     [n ]n Go to git/hg confict marker
  "     yow - toggle wrap
  Plug 'tpope/vim-unimpaired'
  Plug 'preservim/vimux'

  " -- Snippets

  Plug 'SirVer/ultisnips'
  Plug 'honza/vim-snippets'
  Plug 'muralisc/snippets'

  " -- Colorscheme Plugins
  
  " Some colorscheme tested and conclusion
  " solarized                - Good
  " gruvbox                  - Good
  " apprentice               - Good
  " gotham                   - bad for diff highlight
  " dracula                  - bad for types
  " nord                     - bad for diff highlight
  " onedark                  - GOOD
  " base16-solarized-dark    - GOOD
  " jellybeans               - bad for diff
  " base16-summerfruit-dark  - GOOD
  " catppuccin               - visual highlighting is not easily visible
  Plug 'altercation/vim-colors-solarized'
    set background=dark
  " let g:solarized_termtrans=1
  " let g:solarized_termcolors=256
  Plug 'chriskempson/base16-vim'
  Plug 'morhetz/gruvbox'
  Plug 'romainl/Apprentice'
  call plug#end()
endif
" }}}

"---------------------------------------------------------------------------
" GUI settings
"---------------------------------------------------------------------------

set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guioptions-=L

"---------------------------------------------------------------------------
" Basic Settings 
"---------------------------------------------------------------------------
" {{{

" Enable filetype detection
filetype on
" Enable filetype-specific indenting
filetype indent on
" Enable filetype-specific plugins
filetype plugin on
" syntax highlight
syntax on
" prevent screen flasing on multiple esc
set vb t_vb=
" set 256 colors in vim
set t_Co=256
" A little bit more time for macros
set timeoutlen=1200 
" Make Esc work faster
set ttimeoutlen=50  
" don't wrap lines
set nowrap
" use multiple of shiftwidth when indenting with '<' and '>'
set shiftround
" read/write a .viminfo file, don't store more than 80 lines of registers
set viminfo='500,<80
set textwidth=80
set modeline
" always use a fast terminal
set ttyfast
set nospell spelllang=en_us
" Github limit
set colorcolumn=80,132
" default split method is to split in a verical split
set diffopt+=vertical
set dictionary=/usr/share/dict/cracklib-small
" seach for tags|my-tags and bubble up till home direcotry
set tags=tags;~,my-tags;~
" to make restore_view work well
set viewoptions-=options
" allow the cursor to go in to 'invalid' places
set virtualedit=block
" enable using the mouse if terminal emulator supports it (xterm does)
set mouse=a
" Settings for choosing an EOL setting for a file
set fileformats="unix,dos,mac"
" When wrapping paragraphs, don't end lines with 1-letter words (looks stupid)
set formatoptions+=1
" dont consided zero padded numbers as octal or hex (<C-a> and <C-x> works well !! )
set nrformats=
" hide buffers instead of closing them with unwritten changes
set hidden
" reveal already opened files instead of opening new buffers
set switchbuf=useopen

" --- Search settings

" {{{
" show search matches as you type
set incsearch
" search/replace 'globally' (on a line) by default
set gdefault
" ignore case when searching
set ignorecase
" ignore case if search pattern is all lowercase, case-sensitive otherwise
set smartcase
" highlight search terms
set hlsearch                                                                    
" }}}

" --- Vim commands

" {{{

" tab completion for files/buffers in vim commands like bash
set wildmenu
" Complete till the longest match in command
set wildmode=longest,full                        
set wildignore=*.swp,*.bak,*.pyc,*.class
set wildignore+=*/node_modules/*
" ignore case while filename complete
set wildignorecase
" don't beep
set visualbell
" don't beep
set noerrorbells
" show (partial) command at right bottom; this also shows visual selection info
set showcmd
" remember more commands and search history
set history=1000
" }}}

" --- Editing - Basic Settings influencing edition behavior

" {{{
set nofixendofline
" show invisible charecters
set list
" If 'set list' is enabled, the invisible characters are show using listchars
set listchars=tab:▸\ ,trail:·,extends:#,nbsp:·
" a tab is four spaces
set tabstop=4
" when hitting <BS>, delete 4 spaces insted of 1
set softtabstop=4
" expand tabs by default (overloadable per file type later)
set expandtab
" insert tabs on the start of a line according to shiftwidth, not tabstop
set smarttab
" number of spaces to use for autoindenting
set shiftwidth=4
" always set autoindenting on
set autoindent
" copy the previous indentation on autoindenting
set copyindent
set clipboard=unnamedplus
" allow backspacing over everything in insert mode
set backspace=indent,eol,start
if has("unix")
  let s:uname = system("uname -s")
  if s:uname == "Darwin\n"
    set clipboard=unnamed
  endif
endif
if has('mac')
  set guifont=FreeMono:h16
endif
" While pasting, if pasting in insert mode using OS/tmux paste command
" The pasted data will be auto intended giving undesired outcome.
" Either use
"   normal mode paste from "+y 
" Or
" press <F2> to go to 'paste' mode
" set pastetoggle=<F2>
" }}}

" --- UI - Settings influencing UI behaviors 

" {{{
" Read a changed file on disk
set autoread
" set show matching parenthesis
set showmatch
" always show line numbers
set number
" relative number, dissabling use search instead of relative number
" set rnu
" Switch between relative and non relative numbers when focus is changed
" au FocusLost * :set norelativenumber
" au FocusGained * :set relativenumber
set termencoding=utf-8
set encoding=utf-8
" underline the current line, for quick orientation
set cursorline
" have a vertical line marking the cursor column
set cursorcolumn
" keep 0 lines off the edges of the screen when scrolling
set scrolloff=0
" https://shapeshed.com/vim-netrw/
" Split Vertical
let g:netrw_preview = 1
let g:netrw_winsize = 15
" always show what mode we're currently editing in
set showmode
" }}}

" --- Undo/'Getting lost things' settings

" {{{
" use many muchos levels of undo
set undolevels=1000
" do not keep backup files, it's 70's style
set nobackup
" do not write annoying intermediate swap files,
set noswapfile
" keep an undo file (undo changes after closing)
set undofile
set undodir=~/.vim/vimundo
" store swap files in one of these directories (in case swapfile is ever turned on)
set directory=~/.vim/.tmp,/tmp
" }}}

"}}} Basic Settings

" Folding Rules {{{

" enable folding
set foldenable
set foldcolumn=0                                                                " add a fold column
set foldmethod=marker                                                           " detect triple-{ style fold markers [marker indent]
set foldlevel=99                                                                " 0-foldall 99-unfoldall
set foldopen=block,hor,insert,jump,mark,percent,quickfix,search,tag,undo        " which commands trigger auto-unfold
" }}} Folding Rules

" Editor Layout {{{
set lazyredraw                                                                  " don't update the display while executing macros
set laststatus=2                                                                " always put a status line even if one window
set cmdheight=1                                                                 " use a status bar that is 2 rows high
" }}} Editor Layout

"---------------------------------------------------------------------------
" Shortcut Mappings
"---------------------------------------------------------------------------
" {{{
" Resize
nnoremap <Up>    5<c-w>+
nnoremap <Down>  5<c-w>-
nnoremap <Right> 5<c-w>>
nnoremap <Left>  5<c-w><
" Dont move your fingers from the home row OR use ctrl-[ instead `
inoremap jj <Esc>
" Thanks to Steve Losh for this liberating tip[perl/python compatible regex]
" See http://stevelosh.com/blog/2010/09/coming-home-to-vim
nnoremap / /\v
vnoremap / /\v
" never go into Ex mode
nnoremap Q <nop>
nnoremap Y y$
" instead of scorlling to middle .. scroll almost to top
nnoremap zz zt5<C-y>

" Leader mapings 
" {{{

" Change the mapleader from \ to
let mapleader="\<Space>"
let maplocalleader="\<Space>"
" Clears the search register
nnoremap <leader>n :nohlsearch<CR>
" open another file in same dir as current file, Using keymaps from spacemacs
nnoremap <leader>ff :e %:h/<C-d>
" Quit Files with leader + bd
nnoremap <leader>bd :bp\|bd #<cr>
" Easy spliting
nnoremap <leader>s :sp<CR>
nnoremap <leader>v :vs<CR>
" Fast saving
nnoremap <leader>fs :w<cr>
" After yanking in visual mode move cursor to the end of  the selection
vnoremap y ygv<Esc>
" }}}

" Clipboard madness {{{
map p <Plug>(miniyank-autoput)
map P <Plug>(miniyank-autoPut)
" replace currently selected text with default register without yanking it
vnoremap <leader>P "_dP
" }}}
 
" }}}

" Filetype Specific Settings {{{
augroup FTOptions
  autocmd!
  autocmd Filetype xml,xsd,html,javascript,yaml setlocal shiftwidth=2 softtabstop=2 tabstop=2 nostartofline
  autocmd FileType xdefaults                    setlocal commentstring=!\ %s
  autocmd Filetype c,cpp,java,go                setlocal foldmethod=syntax foldlevel=99 complete-=k shiftwidth=2
  autocmd FileType liquid,text,txt,tex          setlocal complete+=k textwidth=80
  autocmd Filetype vim                          setlocal foldmethod=marker keywordprg=:help shiftwidth=2
  autocmd Filetype sh                           setlocal keywordprg=man shiftwidth=2
  autocmd Filetype xml,sh,vim,tex,html,lua      setlocal foldmethod=marker foldlevel=99
  autocmd Filetype gitcommit                    setlocal spell textwidth=72
  autocmd FileType git,gitcommit                setlocal foldmethod=syntax tw=72 cc=+1 spell
  autocmd Filetype markdown                     setlocal iskeyword+=# textwidth=80
augroup end
augroup gitsetup
        autocmd!
        " Only set these commands up for git commits
        autocmd FileType gitcommit
                \ autocmd CursorMoved,CursorMovedI *
                        \ let &l:textwidth = line('.') == 1 ? 50 : ( line('.') == 2 ? 1 : 72)
augroup end
"}}} Filetype Specific Settings

"---------------------------------------------------------------------------
" Plugin Specific Settings
"---------------------------------------------------------------------------
" {{{

" --- For derekwyatt/vim-fswitch

noremap <leader>a :FSHere<CR>

" --- For junegunn/fzf.vim

let g:fzf_preview_window = []
" command! -bang -nargs=* Rg
"       \ call fzf#vim#grep(
"       \   'rg --sortr path --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
"       \   <bang>0 ? fzf#vim#with_preview('up:60%')
"       \           : fzf#vim#with_preview('right:50%:hidden', '?'),
"       \   <bang>0)
" Find in files:
nnoremap <leader>/ :Rg!
" Find in files with word under cursor
nnoremap <leader>* :Rg!<C-R><C-W>
" FZF is faster than CtrlP for finding files in Directories (pf - after projectile find, using same as spacemacs)
nnoremap <leader>pf :FZF! +s --tac <CR>
" Quick open recent file
" Previousl => :CtrlPMRUFiles <CR>, Using keymaps from spacemacs
" See: https://develop.spacemacs.org/doc/DOCUMENTATION.html
nnoremap <leader>fr :History <CR>

" --- For itchyny/lightline.vim

let g:lightline = {
    \ 'colorscheme': 'ayu_dark',
    \ 'component_function': {
    \   'filename': 'LightLineFilename'
    \ },
    \ }
function! LightLineFilename()
  " Get shrinked current working directory and filename
  return  substitute(getcwd(), '\(/.\)\([^/]*\)' , "\\1", "g") . ' | ' . expand('%')
endfunction

" --- For nvim-tree/nvim-tree.lua

" Using same mapping as spacemacs for opening treemacs
nnoremap <leader>fn :NvimTreeFindFile<cr>

" --- For akinsho/toggleterm.nvim

if has("nvim")
  hi ActiveTerminal ctermbg=232 ctermfg=251
  augroup WindowManagement
    autocmd!
    autocmd TermOpen * call Handle_Win_Enter()
  augroup END
  " Change highlight group of terminal window
  function! Handle_Win_Enter()
    setlocal winhighlight=Normal:ActiveTerminal
  endfunction
end

" --- For SirVer/ultisnips

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsListSnippets="<c-tab>"

" --- For wincent/vim-clipper

let g:ClipperPort=5556
let g:fzf_preview_window = ['right:50%', 'ctrl-/']

" --- For powerman/vim-plugin-viewdoc

let g:ViewDoc_DEFAULT = 'ViewDoc_help'
if filereadable(glob("~/.vimrc.local"))
    source ~/.vimrc.local
endif

" --- For airblade/vim-rooter

let g:rooter_silent_chdir = 1
let g:rooter_change_directory_for_non_project_files = 'current'

" --- For christoomey/vim-tmux-navigator

" For compatability with tmux
" Using Meta-[hjkl] mappings in tmux to move panes
let g:tmux_navigator_no_mappings = 0

" }}}

"---------------------------------------------------------------------------
" Set Colorscheme
"---------------------------------------------------------------------------

silent! colorscheme gruvbox
if &diff
    colorscheme gruvbox
endif

"---------------------------------------------------------------------------
" LUA settings
"---------------------------------------------------------------------------

if has("nvim")
lua <<EOF
local opts = { noremap=true, silent=true }
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)

local on_attach = function(client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local bufopts = { noremap=true, silent=true, buffer=bufnr }
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
  vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
  vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
  vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
  vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
  vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
  vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
  vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
  vim.keymap.set('n', '<space>f', function() vim.lsp.buf.format { async = true } end, bufopts)
end

require'lspconfig'.clangd.setup{
  cmd =  { "clangd", "--background-index" }
}
require("lspconfig").lua_ls.setup({
    settings = {
        Lua = {
            telemetry = { enable = false },
            diagnostics = {
                -- Get the language server to recognize the `vim` global
                globals = { "vim" },
            },
        },
    },
    on_attach = on_attach,
})

require("nvim-tree").setup()
require'nvim-treesitter.configs'.setup {
  ensure_installed = { "c", "cpp", "lua", "rust", "python" },
  ignore_install = { "javascript", "verilog" },
  highlight = {
    enable = true,
    disable = { "java", "verilog" },
  },
}
require("toggleterm").setup{
  size = function(term)
    if term.direction == "horizontal" then
      return 15
    elseif term.direction == "vertical" then
      return vim.o.columns * 0.5
    end
  end,
  open_mapping = [[<c-t>]],
  shade_terminals = true,
  shading_factor = 9,
  persist_size = true,
  direction = 'vertical',
  start_in_insert = false,
}
function _G.set_terminal_keymaps()
  local opts = {noremap = true}
  vim.api.nvim_buf_set_keymap(0, 't', '<C-j>', [[<C-\><C-n>]], opts)
  vim.api.nvim_buf_set_keymap(0, 't', '<A-h>', [[<C-\><C-n><C-W>h]], opts)
  vim.api.nvim_buf_set_keymap(0, 't', '<A-j>', [[<C-\><C-n><C-W>j]], opts)
  vim.api.nvim_buf_set_keymap(0, 't', '<A-k>', [[<C-\><C-n><C-W>k]], opts)
  vim.api.nvim_buf_set_keymap(0, 't', '<A-l>', [[<C-\><C-n><C-W>l]], opts)
end

-- if you only want these mappings for toggle term use term://*toggleterm#* instead
vim.cmd('autocmd! TermOpen term://* lua set_terminal_keymaps()')

require('leap').set_default_keymaps()
-- color help from : https://vim.fandom.com/wiki/Xterm256_color_names_for_console_Vim
vim.api.nvim_set_hl(0, 'LeapLabelPrimary', { ctermbg=111, ctermfg=016 , bold = true})
vim.api.nvim_set_hl(0, 'LeapLabelSecondary', { ctermbg=046, ctermfg=016})
EOF
endif
