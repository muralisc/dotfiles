"Courtsey
"Vincent Driessen <vincent@datafox.nl> http://nvie.com/posts/how-i-boosted-my-vim/
"Tsung-Hsiang (Sean) Chang <vgod@vgod.tw>
"and Vim User Manual
"The commands are are anged in the order encountered in vim user manual

set nocompatible                                                                " not compatible with the old-fashion vi mode
" Vundle setup {{{
filetype off                                                                    " Required Vundle setup
set rtp+=~/.vim/bundle/Vundle.vim
set rtp+=~/.fzf
call vundle#begin()
Plugin 'airblade/vim-gitgutter'
Plugin 'chazy/cscope_maps'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'muralisc/vim-colorschemes'
Plugin 'gmarik/Vundle.vim'                                                      " pluging shortcuts
Plugin 'kien/ctrlp.vim'
Plugin 'octol/vim-cpp-enhanced-highlight'                                       " highlighting for STL
Plugin 'powerman/vim-plugin-viewdoc'
Plugin 'tpope/vim-commentary'                                                   " map: gcc
Plugin 'tpope/vim-fugitive'                                                     " GIT
Plugin 'tpope/vim-surround'                                                     " map: ys{tobj}[>)}] - for no space
Plugin 'tpope/vim-unimpaired'                                                   " shorcut for various toggles
call vundle#end()
"}}} ===========================================================Vundle setup done
" Gui options {{{
set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guifont=UbuntuMono\ 20
au FocusLost * :set norelativenumber
au FocusGained * :set relativenumber
" }}} Vundle setup
" Basic Settings {{{
filetype on                                                                     " Enable filetype detection
filetype indent on                                                              " Enable filetype-specific indenting
filetype plugin on                                                              " Enable filetype-specific plugins
syntax on                                                                       " syntax highlight
set t_Co=256                                                                    " set 256 colors in vim
set background=dark
let mapleader="\<Space>"                                                        " Change the mapleader from \ to
set autoread                                                                    " read a changed file on disk
set showmode                                                                    " always show what mode we're currently editing in
set timeoutlen=1200 " A little bit more time for macros
set ttimeoutlen=50  " Make Esc work faster
set nowrap                                                                      " don't wrap lines
"    Editing {{{ "
set tabstop=4                                                                   " a tab is four spaces
set softtabstop=4                                                               " when hitting <BS>, delete 4 spaces insted of 1
set expandtab                                                                   " expand tabs by default (overloadable per file type later)
set shiftwidth=4                                                                " number of spaces to use for autoindenting
set autoindent                                                                  " always set autoindenting on
set copyindent                                                                  " copy the previous indentation on autoindenting
set clipboard=unnamedplus
" }}}    Editing "
set shiftround                                                                  " use multiple of shiftwidth when indenting with '<' and '>'
set backspace=indent,eol,start                                                  " allow backspacing over everything in insert mode
set ignorecase                                                                  " ignore case when searching
" Visual {{{ "
set showmatch                                                                   " set show matching parenthesis
set number                                                                      " always show line numbers
set rnu                                                                         " relative number
set listchars=tab:▸\ ,trail:·,extends:#,nbsp:·
set cursorline                                                                  " underline the current line, for quick orientation
set cursorcolumn                                                                " have a vertical line marking the cursor column
set scrolloff=0                                                                 " keep 4 lines off the edges of the screen when scrolling
set hlsearch                                                                    " highlight search terms
" }}} Visual "
set smartcase                                                                   " ignore case if search pattern is all lowercase, case-sensitive otherwise
set smarttab                                                                    " insert tabs on the start of a line according to shiftwidth, not tabstop
" set virtualedit=block             " (handled by plugin) allow the cursor to go in to 'invalid' places
set incsearch                                                                   " show search matches as you type
set gdefault                                                                    " search/replace 'globally' (on a line) by default
" set nolist                                                                    " don't show invisible characters by default
set list                                                                        " show invisible charecters
" set pastetoggle=<F2>                                                          " Use normal mode paste from "+y (press <F2> to go to 'paste' mode,( prevent auto indenting ))
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
set undodir=~/.vim/vimundo
set directory=~/.vim/.tmp,/tmp                                                  " store swap files in one of these directories (in case swapfile is ever turned on)
set viminfo='20,\"80                                                            " read/write a .viminfo file, don't store more than 80 lines of registers
" Ease of Use {{{ "
set wildmenu                                                                    " tab completion for files/buffers like bash
set wildmode=list:full                                                          " show a list when pressing tab and complete first full match
set wildignore=*.swp,*.bak,*.pyc,*.class
set wildignorecase                                                              " ignore case while filename complete
set title                                                                       " change the terminal's title
set visualbell                                                                  " don't beep
set noerrorbells                                                                " don't beep
set showcmd                                                                     " show (partial) command at right bottom; this also shows visual selection info
" }}} Ease of Use "
set nomodeline                                                                  " disable mode lines (security measure)
set ttyfast                                                                     " always use a fast terminal
set spell spelllang=en_us
set nospell
set colorcolumn=81                                                              " show a marker at 81 so you have a visual cue
set diffopt+=vertical                                                           " default split method is to split in a verical split
set dictionary=/usr/share/dict/cracklib-small
"}}} Basic Settings
" Folding Rules {{{
set foldenable                                                                  " enable folding
set foldcolumn=0                                                                " add a fold column
set foldmethod=syntax                                                           " detect triple-{ style fold markers [marker indent]
set foldlevel=0              " start out with everything folded
set foldopen=block,hor,insert,jump,mark,percent,quickfix,search,tag,undo        " which commands trigger auto-unfold
" Foldingtext {{{ 
" http://www.gregsexton.org/2011/03/improving-the-text-displayed-in-a-fold/ 
 fu! CustomFoldText()
     "get first non-blank line
     let fs = v:foldstart
     while getline(fs) =~ '^\s*$' | let fs = nextnonblank(fs + 1)
     endwhile
     if fs > v:foldend
         let line = getline(v:foldstart)
     else
         let line = substitute(getline(fs), '\t', repeat(' ', &tabstop), 'g')
     endif
     let w = winwidth(0) - &foldcolumn - (&number ? 8 : 0)
     let foldSize = 1 + v:foldend - v:foldstart
     let foldSizeStr = " " . foldSize . " lines "
     let foldLevelStr = repeat("+--", v:foldlevel)
     let lineCount = line("$")
     let foldPercentage = printf("[%.1f", (foldSize*1.0)/lineCount*100) . "%] "
     let expansionString = repeat(".", w - strwidth(foldSizeStr.line.foldLevelStr.foldPercentage))
     return line . expansionString . foldSizeStr . foldPercentage . foldLevelStr
 endf
 set foldtext=CustomFoldText()
" }}} Foldingtext
" }}} Folding Rules
" Editor Layout {{{
set termencoding=utf-8
set encoding=utf-8
set lazyredraw                                                                  " don't update the display while executing macros
set laststatus=2                                                                " always put a status line even if one window
set statusline=[%n]\ %<%.99f\ %h%w%m%r%y%*%=%-14.(%l,%c\|%L%)%{g:colors_name}\ %P
set cmdheight=2                                                                 " use a status bar that is 2 rows high
" }}} Editor Layout
" Shortcut Mappings {{{
" insert mode {{{
" inoremap <c-e> <C-o>A ------  use o 
" Dont move your fingers from the home row OR use ctrl-[ instead
inoremap jj <Esc>
" }}} insert mode 
" normal mappings {{{
" Do something usefull with the arrow keys 
nnoremap <left>  :vertical resize -5<cr> 
nnoremap <right> :vertical resize +5<cr>
nnoremap <up>   :resize -5<cr>
nnoremap <down> :resize +5<cr>
 
nnoremap <F3> :colorscheme random<CR>
" load vimrc
nnoremap <F4> :e ~/dotfiles/.vimrc<CR>
nnoremap <F5> :!cscope -Rbi cscopeFiles<CR>:cs reset<CR>
" Thanks to Steve Losh for this liberating tip[perl/python compatible regex]
" See http://stevelosh.com/blog/2010/09/coming-home-to-vim
nnoremap / /\v
vnoremap / /\v
" never go into Ex mode
nnoremap Q <nop>
nnoremap Y y$
" prevent automatically closing vim
nnoremap ZZ :close<CR>
" }}}  normal mappings
" leader mapings {{{
" Clears the search register
nnoremap <leader>/ :nohlsearch<CR>
nnoremap <Leader>a zA
nnoremap <leader>b :CtrlPBuffer<CR>
nnoremap <leader><C-P> :CtrlP<CR>
" with vimgrep, see results in cope(leader+cc) next (]q) previous ([q)
nnoremap <leader>co :botright cope<cr>
nnoremap <leader>cc :cclose<cr>
" Switch CWD to the directory of the open buffer
nnoremap <leader>cd :cd %:p:h<cr>:pwd<cr>
nnoremap <Leader>dd :Gdiff<CR>
nnoremap <Leader>du :diffupdate<CR>
" ge is used after n_CTRL-e
nnoremap <Leader>ge :GitGutterNextHunk<CR>
" ge is used after n_CTRL-y
nnoremap <Leader>gy :GitGutterPrevHunk<CR>
" make file ( use quick fix window to see errors )
nnoremap <leader>m :!clear<CR>:w<CR>:make<CR>
" open another file in same dir as current file
nnoremap <leader>o :e %:h/<C-d>
" clipboard madness {{{
" paste from the primary clipboard/ selection
nnoremap <leader>P "*p
" paste from the secondary clipboard/ ctrl+c
nnoremap <leader>p "+p
" yank from the primary clipboard/ selection
nnoremap <leader>Y "*y
" yank from the secondary clipboard/ ctrl+c
nnoremap <leader>y "+y
" }}} clipboard madness
" Quit Files with ldr + q
nnoremap <leader>q :bd<cr>
nnoremap <leader>Q :qall!<cr>
nnoremap <leader>r :so $MYVIMRC<CR>
nnoremap <leader>s :Gstatus<CR>
" traling spaces and jump to last point
nnoremap <leader>t :%s/\s\+$//e<cr>`'
" Useful mappings for managing tabs
nnoremap <leader>T :tabnew<cr>
nnoremap <leader>u :Unite -start-insert line<CR>
" Open vimgrep and put the cursor in the right position
nnoremap <leader>v :vimgrep // **/*.<left><left><left><left><left><left><left>
" Fast saving
nnoremap <leader>w :w<cr>
" Sudo to write
nnoremap <leader>W w !sudo tee % >/dev/null
" make the current file executable
nnoremap <silent><leader>x :!chmod +x %<CR>
nnoremap <Leader>zz :let &scrolloff=999-&scrolloff<CR>
" }}} leader maping end
" }}} Shortcut Mappings 
" Plugin Specific Settings {{{
set tags=./tags;~/Projects
let g:ViewDoc_DEFAULT = 'ViewDoc_help'
" YOU COMPLETE ME
let g:ycm_confirm_extra_conf = 0
let g:ycm_always_populate_location_list = 1
" PYTHON MODE SETTINGS
let g:pymode_rope = 1
" PYMODE ignore errors list
let g:pymode_lint_ignore = "E702,E501,E225,E221,E203,E231,E201,E202,E261,E262"
let g:ctrlp_cmd = 'CtrlPMRUFiles'
" VIM-FIGITIVE : close all unwanted buffers opened by vim fugitive (git blame)
" autocmd BufReadPost fugitive://* set bufhidden=delete
" delimitMate settings
let delimitMate_expand_space = 1
let delimitMate_expand_cr = 1
" ULTISNIPS
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-tab>"
let g:UltiSnipsEditSplit="vertical"
" }}} Plugin Specific Settings 
" Filetype Specific Settings {{{
                                                                                " Restore cursor position upon reopening files
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif
                                                                                 " Strip all trailing whitespace from a file
augroup Misc " {{{2
    autocmd!
    autocmd BufNewFile,BufRead *.txt,README,INSTALL,NEWS,TODO if &ft == ""|set ft=text|endif
augroup END " }}}2
augroup FTOptions
    autocmd!
    autocmd filetype javascript setlocal shiftwidth=2 softtabstop=2
    autocmd FileType xml,xsd,xslt,javascript setlocal tabstop=2
    autocmd FileType xdefaults setlocal commentstring=!\ %s
    autocmd FileType matlab setlocal commentstring=%\ %s
    autocmd filetype c,cpp,java setlocal foldmethod=syntax foldlevel=99
    autocmd FileType c,cpp,java setlocal complete-=k                                                                 " add dictionary too
    autocmd FileType liquid,markdown,text,txt setlocal complete+=k
    autocmd filetype vim setlocal keywordprg=:help
    autocmd filetype sh setlocal keywordprg=man
    autocmd filetype xml,sh,vim,tex,html,lua setlocal foldmethod=marker
    autocmd FileType gitcommit,tex setlocal spell
    autocmd FileType git,gitcommit setlocal foldmethod=syntax foldlevel=1
    autocmd FileType liquid,markdown,text,txt setlocal tw=100 linebreak nolist
augroup end
"}}} Filetype Specific Settings 

"{{{ load colorscheme depending on the day of month
fu! s:LoadRandomColorScheme() 

    let s:color_file_list = globpath(&runtimepath, 'colors/*.vim'     )
    let s:color_file_list = substitute(s:color_file_list, '\'            , '/', 'g')
    let s:color_file_list = substitute(s:color_file_list, "\n"           , ',', 'g')
    let s:color_file_list = substitute(s:color_file_list, '\(/[^,]\+/\)' , '', 'g')
    let s:color_file_list = substitute(s:color_file_list, '\.vim' , '', 'g')
    " echo s:color_file_list

    if strlen(s:color_file_list)
        if s:color_file_list =~ ','
            let s:rnd  = (strftime( "%d" ) + 0)%12
            let s:loop = 0

            while s:loop < s:rnd
                let s:color_file_list = substitute(s:color_file_list, '^\([^,]\+\),', '', '')
                let s:loop            = s:loop + 1
            endwhile
            " echo s:color_file_list

            let s:color_file = matchstr(s:color_file_list, '^[^,]\+')
            " echo s:color_file
            execute "colorscheme" s:color_file
            unlet! s:color_file

            unlet! s:loop 
            unlet! s:rnd 
        endif
    endif

    unlet! s:color_file_list 
    unlet! s:self_file
endf "}}}
call s:LoadRandomColorScheme()

nnoremap <leader>l :!firefox <C-R><C-A><CR>
" prevent screen flasing on multiple esc
set vb t_vb=
