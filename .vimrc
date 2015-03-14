"Courtsey Vincent Driessen <vincent@datafox.nl>
"http://nvie.com/posts/how-i-boosted-my-vim/
"Tsung-Hsiang (Sean) Chang <vgod@vgod.tw>
"and me
set nocompatible                " not compatible with the old-fashion vi mode
"{{{ Vundle setup
filetype off                    " Required Vundle setup
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-fugitive'
Plugin 'edsono/vim-matchit'
Plugin 'gregsexton/gitv'
Plugin 'biskark/vim-ultimate-colorscheme-utility'
Plugin 'flazz/vim-colorschemes'
Plugin 'takac/vim-hardtime'
Plugin 'tpope/vim-unimpaired'
Plugin 'junegunn/vim-easy-align'
Plugin 'kien/ctrlp.vim'
" ================================================LATEX
" yaourt -S vim-latexsuite-git
" ================================================Python
Plugin 'davidhalter/jedi-vim'
Plugin 'klen/python-mode'
" ================================================CPP
"  yaourt -S vim-youcompleteme-git
Plugin 'octol/vim-cpp-enhanced-highlight'       " highlighting for STL
Plugin 'chazy/cscope_maps'
call vundle#end()
"}}} ================================================Vundle setup done
" Basic Settings{{{
filetype on                     " Enable filetype detection
filetype indent on              " Enable filetype-specific indenting
filetype plugin on              " Enable filetype-specific plugins
syntax on                       " syntax highlight
set t_Co=256                    " set 256 colors in vim
colorscheme lettuce
"let mapleader=","              " Change the mapleader from \ to ,
set showmode                    " always show what mode we're currently editing in
set nowrap                      " don't wrap lines
set tabstop=4                   " a tab is four spaces
set softtabstop=4               " when hitting <BS>, delete 4 spaces insted of 1
set expandtab                   " expand tabs by default (overloadable per file type later)
set shiftwidth=4                " number of spaces to use for autoindenting
set shiftround                  " use multiple of shiftwidth when indenting with '<' and '>'
" set backspace=indent,eol,start  " allow backspacing over everything in insert mode
set backspace=0                 " for hard mode
set autoindent                  " always set autoindenting on
set copyindent                  " copy the previous indentation on autoindenting
set number                      " always show line numbers
set rnu                         " relative number
set showmatch                   " set show matching parenthesis
set ignorecase                  " ignore case when searching
set smartcase                   " ignore case if search pattern is all lowercase,
                                "    case-sensitive otherwise
set smarttab                    " insert tabs on the start of a line according to
                                "    shiftwidth, not tabstop
"set scrolloff=4                " keep 4 lines off the edges of the screen when scrolling
set virtualedit=block            " allow the cursor to go in to "invalid" places
set hlsearch                    " highlight search terms
set incsearch                   " show search matches as you type
set gdefault                    " search/replace "globally" (on a line) by default
set listchars=tab:▸\ ,trail:·,extends:#,nbsp:·
" set nolist                    " don't show invisible characters by default
                                " but it is enabled for some file types (see later)
set list                        " show invisible charecters
set pastetoggle=<F2>            " when in insert mode, press <F2> to go to
                                "    paste mode, where you can paste mass data
                                "    that won't be autoindented
set mouse=a                     " enable using the mouse if terminal emulator
                                "    supports it (xterm does)
set fileformats="unix,dos,mac"
set formatoptions+=1            " When wrapping paragraphs, don't end lines
                                "    with 1-letter words (looks stupid)
set nrformats=                  " make <C-a> and <C-x> play well with
                                "    zero-padded numbers (i.e. don't consider
                                "    them octal or hex)
set hidden                      " hide buffers instead of closing them this
                                "    means that the current buffer can be put
                                "    to background without being written; and
                                "    that marks and undo history are preserved
set switchbuf=useopen           " reveal already opened files from the
                                " quickfix window instead of opening new
                                " buffers
set history=1000                " remember more commands and search history
set undolevels=1000             " use many muchos levels of undo
set nobackup                    " do not keep backup files, it's 70's style
set noswapfile                  " do not write annoying intermediate swap files,
set undofile                    " keep an undo file (undo changes after closing)
set undodir=/var/tmp/vimundo
set directory=~/.vim/.tmp,~/tmp,/tmp
                                " store swap files in one of these directories
                                "    (in case swapfile is ever turned on)
set viminfo='20,\"80            " read/write a .viminfo file, don't store more
                                "    than 80 lines of registers
set wildmenu                    " tab completion for files/buffers like bash
set wildmode=list:full          " show a list when pressing tab and complete
                                "    first full match
set wildignore=*.swp,*.bak,*.pyc,*.class
set wildignorecase              " ignore case while filename complete
set title                       " change the terminal's title
set visualbell                  " don't beep
set noerrorbells                " don't beep
set showcmd                     " show (partial) command in the last line of the screen
                                "    this also shows visual selection info
set nomodeline                  " disable mode lines (security measure)
"set ttyfast                    " always use a fast terminal
set cursorline                  " underline the current line, for quick orientation
set cursorcolumn                " have a vertical line marking the cursor column
set spell spelllang=en_us
set nospell
set colorcolumn=81              " show a marker at 81 so you have a visual cue
"}}}
" Folding rules {{{
set foldenable                  " enable folding
set foldcolumn=2                " add a fold column
set foldmethod=marker           " detect triple-{ style fold markers [marker indent]
set foldlevelstart=99           " start out with everything folded
set foldopen=block,hor,insert,jump,mark,percent,quickfix,search,tag,undo
                                " which commands trigger auto-unfold
" }}}
" Editor layout {{{
set termencoding=utf-8
set encoding=utf-8
set lazyredraw                  " don't update the display while executing macros
set laststatus=2                " always put a status line even if one window
set statusline=[%n]\ %<%.99f\ %h%w%m%r%y%*%=%-14.(%l,%c\|%L%)\ %P
set cmdheight=2                 " use a status bar that is 2 rows high
" }}}
" Shortcut mappings {{{

" Thanks to Steve Losh for this liberating tip[perl/python compatible regex]
" See http://stevelosh.com/blog/2010/09/coming-home-to-vim
nnoremap / /\v
vnoremap / /\v
" never go into Ex mode
nnoremap Q <nop>
" Use the damn hjkl keys
 noremap <up> <nop>
 noremap <down> <nop>
 noremap <left> <nop>
 noremap <right> <nop>
"for i mode also
 inoremap <up> <nop>
 inoremap <down> <nop>
 inoremap <left> <nop>
 inoremap <right> <nop>
" toggle HARDMODE
nnoremap <leader>h <Esc>:HardTimeToggle<CR>
" Clears the search register
nnoremap <silent> <leader>c :nohlsearch<CR>
" Pull word under cursor into substitute (for quick search and replace)
nnoremap <leader>z :%s#<C-r>=expand("<cword>")<CR>#
" Strip all trailing whitespace from a file, using ,w
nnoremap <leader>w :%s/\s\+$//<CR>:let @/=''<CR>
" Dont move your fingers from the home row (either use 'jj' or 'jk')
" inoremap jj <Esc>     user ctrl-c instead
" load vimrc
nnoremap <F4> :e $MYVIMRC<CR>
" Remap c-x c-o to <c-space> [ @ is used for space in vim ]
" <C-p> is added at the end to prevent it selecting the
" first option by default
inoremap <C-@> <C-x><C-o><C-p>
" Sudo to write
cnoremap <leader>w w !sudo tee % >/dev/null
" open another file in same dir as current file
nnoremap <leader>e :e %:h/<C-d>
" }}}
" Restore cursor position upon reopening files {{{
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif
" }}}
" {{{               plugin specific settings
set tags=/home/mur/.vim/tagsForCtags
" CLANG COMPLETE SETTINGS
let g:clang_complete_auto = 0
let g:clang_use_library = 1
let g:clang_periodic_quickfix = 0
let g:clang_close_preview = 1
let g:clang_library_path = '/usr/lib/'
" YOU COMPLETE ME
let g:ycm_confirm_extra_conf = 0
" If you prefer the Omni-Completion tip window to close when a selection is
" made, these lines close it on movement in insert mode or when leaving
" insert mode
" autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
" autocmd InsertLeave * if pumvisible() == 0|pclose|endif
" PYTHON MODE SETTINGS
let g:pymode_rope = 1
" VIM-FIGITIVE : close all unwanted buffers opened by vim fugitive (git blame)
autocmd BufReadPost fugitive://* set bufhidden=delete
" PYMODE ignore errors list
let g:pymode_lint_ignore = "E702,E501,E225,E221,E203,E231,E201,E202,E261,E262"
" CTRLP
nnoremap <leader>b :CtrlPBuffer<CR>
nnoremap <leader>m :CtrlPMRUFiles<CR>
" HARD Time
let g:hardtime_default_on = 1
" ==}}}
"{{{ Filetype specific settings
autocmd FileType java set tags=~/.tags

augroup javascript_files
    au!
    autocmd filetype javascript setlocal expandtab
    autocmd filetype javascript setlocal listchars=trail:·,extends:#,nbsp:·
    autocmd filetype javascript setlocal shiftwidth=2
    autocmd filetype javascript setlocal softtabstop=2
    autocmd filetype javascript setlocal tabstop=2
augroup end
augroup cpp_files
    au!
    autocmd filetype cpp setlocal foldmethod=marker
    autocmd filetype c setlocal foldmethod=marker
augroup end
augroup vim_files
    au!
    autocmd filetype vim setlocal keywordprg=:help
augroup end
augroup sh_files
    au!
    autocmd filetype sh setlocal keywordprg=man
augroup end
augroup txt_files
    au!
    autocmd FileType text setlocal textwidth=78
augroup end
"}}}
