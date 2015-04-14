"Courtsey 
"Vincent Driessen <vincent@datafox.nl> http://nvie.com/posts/how-i-boosted-my-vim/
"Tsung-Hsiang (Sean) Chang <vgod@vgod.tw>
"and Vim User Manual
"The commands are arranged in the order encountered in vim user manual

set nocompatible                                                                " not compatible with the old-fashion vi mode
"{{{ Vundle setup
filetype off                                                                    " Required Vundle setup
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-commentary'
Plugin 'vim-scripts/matchit.zip'
Plugin 'tpope/vim-unimpaired'                                                   " shorcut for various toggles
Plugin 'kien/ctrlp.vim'
Plugin 'flazz/vim-colorschemes'
Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-colorscheme-switcher'
Plugin 'powerman/vim-plugin-viewdoc'
" ============================================================================GIT
Plugin 'tpope/vim-fugitive'
Plugin 'gregsexton/gitv'
" ==========================================================================LATEX
" yaourt -S vim-latexsuite-git
" =========================================================================Python
Plugin 'davidhalter/jedi-vim'
" Plugin 'klen/python-mode'
" ============================================================================CPP
Plugin 'vim-scripts/Conque-GDB'
"  yaourt -S vim-youcompleteme-git
Plugin 'octol/vim-cpp-enhanced-highlight'                                       " highlighting for STL
Plugin 'chazy/cscope_maps'
call vundle#end()
"}}} ===========================================================Vundle setup done
" Basic Settings{{{
filetype on                                                                     " Enable filetype detection
filetype indent on                                                              " Enable filetype-specific indenting
filetype plugin on                                                              " Enable filetype-specific plugins
syntax on                                                                       " syntax highlight
set t_Co=256                                                                    " set 256 colors in vim
set background=dark
"let mapleader=","                                                              " Change the mapleader from \ to ,
set autoread
set showmode                                                                    " always show what mode we're currently editing in
set nowrap                                                                      " don't wrap lines
set tabstop=4                                                                   " a tab is four spaces
set softtabstop=4                                                               " when hitting <BS>, delete 4 spaces insted of 1
set expandtab                                                                   " expand tabs by default (overloadable per file type later)
set shiftwidth=4                                                                " number of spaces to use for autoindenting
set shiftround                                                                  " use multiple of shiftwidth when indenting with '<' and '>'
set backspace=indent,eol,start                                                  " allow backspacing over everything in insert mode
set autoindent                                                                  " always set autoindenting on
set copyindent                                                                  " copy the previous indentation on autoindenting
set number                                                                      " always show line numbers
set rnu                                                                         " relative number
set showmatch                                                                   " set show matching parenthesis
set ignorecase                                                                  " ignore case when searching
set smartcase                                                                   " ignore case if search pattern is all lowercase, case-sensitive otherwise
set smarttab                                                                    " insert tabs on the start of a line according to shiftwidth, not tabstop
"set scrolloff=4                                                                " keep 4 lines off the edges of the screen when scrolling
set virtualedit=block                                                           " allow the cursor to go in to 'invalid' places
set hlsearch                                                                    " highlight search terms
set incsearch                                                                   " show search matches as you type
set gdefault                                                                    " search/replace 'globally' (on a line) by default
set listchars=tab:▸\ ,trail:·,extends:#,nbsp:·
" set nolist                                                                    " don't show invisible characters by default
set list                                                                        " show invisible charecters
set pastetoggle=<F2>                                                            " press <F2> to go to 'paste' mode,( prevent auto indenting )
set mouse=a                                                                     " enable using the mouse if terminal emulator supports it (xterm does)
set fileformats="unix,dos,mac"
set formatoptions+=1                                                            " When wrapping paragraphs, don't end lines with 1-letter words (looks stupid)
set nrformats=                                                                  " dont consided zero padded numbers as octal or hex (<C-a> and <C-x> works well !! )
set hidden                                                                      " hide buffers instead of closing them with unwritten changes
set switchbuf=useopen                                                           " reveal already opened files instead of opening new buffers
set history=1000                                                                " remember more commands and search history
set undolevels=1000                                                             " use many muchos levels of undo
set nobackup                                                                    " do not keep backup files, it's 70's style
set noswapfile                                                                  " do not write annoying intermediate swap files,
set undofile                                                                    " keep an undo file (undo changes after closing)
set undodir=/var/tmp/vimundo
set directory=~/.vim/.tmp,/tmp                                                  " store swap files in one of these directories (in case swapfile is ever turned on)
set viminfo='20,\"80                                                            " read/write a .viminfo file, don't store more than 80 lines of registers
set wildmenu                                                                    " tab completion for files/buffers like bash
set wildmode=list:full                                                          " show a list when pressing tab and complete first full match
set wildignore=*.swp,*.bak,*.pyc,*.class
set wildignorecase                                                              " ignore case while filename complete
set title                                                                       " change the terminal's title
set visualbell                                                                  " don't beep
set noerrorbells                                                                " don't beep
set showcmd                                                                     " show (partial) command at right bottom; this also shows visual selection info
set nomodeline                                                                  " disable mode lines (security measure)
set ttyfast                                                                     " always use a fast terminal
set cursorline                                                                  " underline the current line, for quick orientation
set cursorcolumn                                                                " have a vertical line marking the cursor column
set spell spelllang=en_us
set nospell
set colorcolumn=81                                                              " show a marker at 81 so you have a visual cue
set complete+=k                                                                 " add dictionary too
set dictionary=/usr/share/dict/cracklib-small
"}}}
" Folding rules {{{
set foldenable                                                                  " enable folding
set foldcolumn=2                                                                " add a fold column
set foldmethod=marker                                                           " detect triple-{ style fold markers [marker indent]
set foldlevelstart=99                                                           " start out with everything folded
set foldopen=block,hor,insert,jump,mark,percent,quickfix,search,tag,undo        " which commands trigger auto-unfold
" }}}
" Editor layout {{{
set termencoding=utf-8
set encoding=utf-8
set lazyredraw                                                                  " don't update the display while executing macros
set laststatus=2                                                                " always put a status line even if one window
set statusline=[%n]\ %<%.99f\ %h%w%m%r%y%*%=%-14.(%l,%c\|%L%)\ %P
set cmdheight=2                                                                 " use a status bar that is 2 rows high
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
                                                                                " make the current file executable
nnoremap <silent><leader>x :!chmod +x %<CR>
                                                                                " Clears the search register
nnoremap <leader>c :nohlsearch<CR>
                                                                                " Pull word under cursor into substitute (for quick search and replace)
nnoremap <leader>z :%s#<C-r>=expand("<cword>")<CR>#
                                                                                " Strip all trailing whitespace from a file, using ,w
nnoremap <leader>w :%s/\s\+$//<CR>:let @/=''<CR>
                                                                                " Dont move your fingers from the home row (either use 'jj' or 'jk') OR use ctrl-[ instead
inoremap jk <Esc>
                                                                                " load vimrc
nnoremap <F4> :e $MYVIMRC<CR>
                                                                                " Remap c-x c-o to <c-space> [ @ is used for space in vim ]
                                                                                " <C-p> is added at the end to prevent it selecting the
                                                                                " first option by default
inoremap <C-@> <C-x><C-o><C-p>
                                                                                " Sudo to write
cnoremap <leader>w! w !sudo tee % >/dev/null
                                                                                " open another file in same dir as current file
nnoremap <leader>e :e %:h/<C-d>
                                                                                " Compile
map <F6> :make<CR>
                                                                                " Debug
map <F5> :ConqueGdbVSplit %:r<CR>
" ============================================================================}}}
" {{{               plugin specific settings
                                                                                " Conque GDB
let g:ConqueGdb_Leader = ','
let g:ConqueTerm_CloseOnEnd = 1
set tags=/home/mur/.vim/tagsForCtags
                                                                                " YOU COMPLETE ME
let g:ycm_confirm_extra_conf = 0
                                                                                " PYTHON MODE SETTINGS
let g:pymode_rope = 1
                                                                                " VIM-FIGITIVE : close all unwanted buffers opened by vim fugitive (git blame)
autocmd BufReadPost fugitive://* set bufhidden=delete
                                                                                " PYMODE ignore errors list
let g:pymode_lint_ignore = "E702,E501,E225,E221,E203,E231,E201,E202,E261,E262"
                                                                                " CTRLP
nnoremap <leader>b :CtrlPBuffer<CR>
nnoremap <leader>m :CtrlPMRUFiles<CR>
" ==}}}
"{{{ Filetype specific settings
                                                                                " Restore cursor position upon reopening files
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

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
    autocmd filetype cpp setlocal makeprg=g++\ -g\ %:p\ -o\ %:r\ -std=c++11
                                                                                " automatically switch numbering -> relative ( helps with ConqueGDB )
    autocmd FileType cpp :autocmd InsertEnter * :windo set norelativenumber
    autocmd FileType cpp :autocmd InsertLeave * :windo set relativenumber
augroup end
augroup vim_files
    au!
    autocmd filetype vim setlocal keywordprg=:help
augroup end
augroup sh_files
    au!
    autocmd filetype sh setlocal keywordprg=man
augroup end
augroup tex_files
    au!
    autocmd FileType tex setlocal makeprg=pdflatex\ %
    autocmd FileType tex setlocal textwidth=78
augroup end
augroup txt_files
    au!
    autocmd FileType text setlocal textwidth=78
augroup end
augroup python_files
    au!
    autocmd FileType python setlocal makeprg=python\ %
augroup end
"}}}
noremap <F3> :NextColorScheme<CR>
noremap <F2> :PrevColorScheme<CR>

colorscheme mustang
