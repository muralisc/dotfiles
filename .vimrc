" vim: foldlevel=2:
" Irrelevent sections are given a foldlevel of 3 so that they are folded by default
" Courtsey :
" Vincent Driessen <vincent@datafox.nl> http://nvie.com/posts/how-i-boosted-my-vim/
" Tsung-Hsiang (Sean) Chang <vgod@vgod.tw>
" and Vim User Manual
" The commands are are anged in the order encountered in vim user manual
let g:loaded_matchparen = 1
set nocompatible                                                     " not compatible with the old-fashion vi mode
" vim-plug setup {{{1
if filereadable(expand("~/.vim/autoload/plug.vim"))
    call plug#begin('~/.vim/plugged')
    Plug 'scrooloose/nerdtree'                                         " Folder navigation ? C u r cd CD                             *  *
    Plug 'w0rp/ale'                                                    " Async Syntax checking (with cpp, rust,shellcheck)                *  *  *  *  *
    Plug 'mileszs/ack.vim'                                             " search files                                                *  *  *  *  *
    Plug 'powerman/vim-plugin-viewdoc'                                 " for viewing help files                                      *  *  *  *  *
    Plug 'tpope/vim-commentary'                                        " map: gcc                                                    *  *  *  *  *
    Plug 'tpope/vim-surround'                                          " map: ys [ <{( >)} ] - for no space                           *  *  *  *  *
    Plug 'tpope/vim-unimpaired'                                        " yon | yor | yow | ]q | [q |                                 *  *  *  *  *
    Plug 'godlygeek/tabular'                                           " for easily aligning                                         *  *  *  *  *
    Plug 'vim-scripts/restore_view.vim'                                "                                                             *  *  *  *  *
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }  " to install fzf in system                                    *  *  *  *  *
    Plug 'junegunn/fzf.vim'                                            " for :Windows :Lines                                  *  *  *  *  *
    Plug 'muralisc/vim-colorschemes'                                   "                                                             *  *  *  *  *
    Plug 'christoomey/vim-tmux-navigator'                              "                                                             *  *  *  *  *
    Plug 'airblade/vim-gitgutter'                                      " for: ]h [h
    Plug 'itchyny/lightline.vim'
    Plug 'airblade/vim-rooter'
    Plug 'fatih/vim-go'
    " Plug 'tpope/vim-fugitive'
    call plug#end()
endif
"}}}1 ===========================================================Vundle setup done
set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guioptions-=L
au FocusLost * :set norelativenumber
au FocusGained * :set relativenumber
" Basic Settings {{{
filetype on                                                                     " Enable filetype detection
filetype indent on                                                              " Enable filetype-specific indenting
filetype plugin on                                                              " Enable filetype-specific plugins
syntax on                                                                       " syntax highlight
set vb t_vb=                                                                    " prevent screen flasing on multiple esc
set t_Co=256                                                                    " set 256 colors in vim
set background=dark
let mapleader="\<Space>"                                                        " Change the mapleader from \ to
set autoread                                                                    " read a changed file on disk
set showmode                                                                    " always show what mode we're currently editing in
set nofixendofline
set timeoutlen=1200 " A little bit more time for macros
set ttimeoutlen=50  " Make Esc work faster
set nowrap                                                                      " don't wrap lines
" Editing {{{
set tabstop=4                                                                   " a tab is four spaces
set softtabstop=4                                                               " when hitting <BS>, delete 4 spaces insted of 1
set expandtab                                                                   " expand tabs by default (overloadable per file type later)
set shiftwidth=4                                                                " number of spaces to use for autoindenting
set autoindent                                                                  " always set autoindenting on
set copyindent                                                                  " copy the previous indentation on autoindenting
set clipboard=unnamedplus
if has("unix")
  let s:uname = system("uname -s")
  if s:uname == "Darwin\n"
    set clipboard=unnamed
  endif
endif
if has('mac')
    set guifont=Hack:h14
endif
" }}}
set shiftround                                                                  " use multiple of shiftwidth when indenting with '<' and '>'
set backspace=indent,eol,start                                                  " allow backspacing over everything in insert mode
set ignorecase                                                                  " ignore case when searching
" Visual {{{
" set showmatch                                                                   " set show matching parenthesis
set number                                                                      " always show line numbers
set rnu                                                                         " relative number
set termencoding=utf-8
set encoding=utf-8
set listchars=tab:▸\ ,trail:·,extends:#,nbsp:·
set cursorline                                                                  " underline the current line, for quick orientation
set cursorcolumn                                                                " have a vertical line marking the cursor column
set scrolloff=0                                                                 " keep 4 lines off the edges of the screen when scrolling
set hlsearch                                                                    " highlight search terms
" }}}
set smartcase                                                                   " ignore case if search pattern is all lowercase, case-sensitive otherwise
set smarttab                                                                    " insert tabs on the start of a line according to shiftwidth, not tabstop
set virtualedit=block                                                           " allow the cursor to go in to 'invalid' places
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
set textwidth=132        " not 80 cause helps in vs mode
" Ease of Use {{{
set wildmenu                                                                    " tab completion for files/buffers like bash
set wildmode=longest,full                                                       " Complete till the longest match in command
set wildignore=*.swp,*.bak,*.pyc,*.class
set wildignore+=*/node_modules/*
set wildignorecase                                                              " ignore case while filename complete
set visualbell                                                                  " don't beep
set noerrorbells                                                                " don't beep
set showcmd                                                                     " show (partial) command at right bottom; this also shows visual selection info
" }}} Ease of Use "
set modeline                                                                    " enable mode lines
set ttyfast                                                                     " always use a fast terminal
set spell spelllang=en_us
set nospell
set colorcolumn=80,132                                                          " Github limit
set diffopt+=vertical                                                           " default split method is to split in a verical split
set dictionary=/usr/share/dict/cracklib-small
set tags=tags;~,my-tags;~                                                       " seach for tags|TAGS|my-tags and bubble up till home direcotry
set viewoptions-=options                                                        " to make restore_view work well
let g:solarized_termcolors=256
silent! colorscheme solarized-custom
"}}} Basic Settings
" Folding Rules {{{
set foldenable                                                                  " enable folding
set foldcolumn=0                                                                " add a fold column
set foldmethod=marker                                                           " detect triple-{ style fold markers [marker indent]
set foldlevel=99                                                                " 0-foldall 99-unfoldall
set foldopen=block,hor,insert,jump,mark,percent,quickfix,search,tag,undo        " which commands trigger auto-unfold
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
" }}} Folding Rules
" Editor Layout {{{
set lazyredraw                                                                  " don't update the display while executing macros
set laststatus=2                                                                " always put a status line even if one window
set cmdheight=1                                                                 " use a status bar that is 2 rows high
" }}} Editor Layout

" Commented for now in favor of lightline
"" Plugin less Statusline settings {{{3
"" https://stackoverflow.com/a/10416234
"" http://got-ravings.blogspot.in/2008/08/vim-pr0n-making-statuslines-that-own.html
"" http://www.calmar.ws/vim/256-xterm-24bit-rgb-color-chart.html
"" https://www.blaenkdenum.com/posts/a-simpler-vim-statusline/
"" https://gist.github.com/XVilka/8346728
"function! Status(winnum)
"  let active = a:winnum == winnr()
"  let bufnum = winbufnr(a:winnum)
"  let stat = ''
"  function! Color(active, num )
"    if a:active
"      return '%' . a:num . '*'
"    else
"      return '%*'
"    endif
"  endfunction
"  " file name
"  let stat .= Color( active, 7 )
"  let stat .= '%<%f%*'
"  " help? preview? modified? readonly?
"  let stat .= Color( active, 1 )
"  let stat .= ' %h%w%m%r%*'
"  " right side
"  let stat .= Color( active, 7 )
"  let stat .= ' %=%*'
"  " Encoding2
"  let stat .= Color( active, 5 )
"  let stat.='%-7(%{&ff}%)%*'
"  " Encoding
"  let stat .= Color( active, 3 )
"  let stat.='%-7(%{&fenc}%)%*'
"  " filetype
"  let stat .= Color( active, 2 )
"  let stat.='%-7(%y%)%*'
"  " %* leftjustify(col,virtulcol) percentage
"  let stat .= Color( active, 9 )
"  let stat.='%-15( %c%V%)%*'
"  return stat
"endfunction
"
""RefreshStatus is used to updated all the status lines
"function! s:RefreshStatus()
"  for nr in range(1, winnr('$'))
"    call setwinvar(nr, '&statusline', '%!Status(' . nr . ')')
"  endfor
"endfunction
"
"augroup status
"  autocmd!
"  autocmd VimEnter,WinEnter,BufWinEnter * call <SID>RefreshStatus()
"augroup END
" }}}3

" Shortcut Mappings {{{1
" resize
nnoremap <Up>    5<c-w>+
nnoremap <Down>  5<c-w>-
nnoremap <Right> 5<c-w>>
nnoremap <Left>  5<c-w><
" search the text selected in visual mode in entire file
vnoremap // y/<C-R>"<CR>
" Dont move your fingers from the home row OR use ctrl-[ instead `
inoremap jj <Esc>
" normal mappings {{{
inoremap <F5> <C-R>=strftime("# %F, %r, %a")<CR>
nnoremap <F6> :redraw!<CR>
" Thanks to Steve Losh for this liberating tip[perl/python compatible regex]
" See http://stevelosh.com/blog/2010/09/coming-home-to-vim
nnoremap / /\v
vnoremap / /\v
" never go into Ex mode
nnoremap Q <nop>
nnoremap Y y$
" prevent automatically closing vim
nnoremap ZZ :close<CR>
" instead of scorlling to middle .. scroll almost to top
nnoremap zz zt5<C-y>
nnoremap <buffer> <Enter> :.cc<CR>:copen<CR>
nnoremap <C-c> :cclose<CR>
" }}}  normal mappings
" leader mapings {{{
" Toggle Fold easily
nnoremap <Leader>a zA
nnoremap <Leader>du :diffupdate<CR>
nnoremap <Leader>gl :silent! Glog --<CR>
" when browsing glog, when you are in diff and want to go back to commit TODO
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <leader>gs :Gstatus<CR>
" Clears the search register
nnoremap <leader>/ :nohlsearch<CR>
" with vimgrep, see results in cope(leader+cc) next (]q) previous ([q)
nnoremap <leader>co :botright cope<cr>
" Open vimGrep and put the cursor in the right position
" nnoremap <leader>gr :Ack! --ignore 'tags' --ignore 'test' <C-r><C-w>
" Use <c-r>" to get copied item
nnoremap <leader>gr :Rg!
" FZF is faster than CtrlP for finding files in Directories
nnoremap <leader>m :FZF<CR>
" nnoremap <leader><leader>m :CtrlPMRUFiles <CR>
nnoremap <leader><leader>m :History <CR>
" NERD
nnoremap <leader>n :NERDTreeFind<CR>
nnoremap <leader><leader>n :NERDTreeToggle<CR>
" open another file in same dir as current file
nnoremap <leader>o :e %:h/<C-d>
" Quit Files with leader + q
nnoremap <leader>q :bp\|bd #<cr>
" Close splits but not last window
nnoremap <leader><leader>q :close!<cr>
" Close vim itself
nnoremap <leader><leader><leader>q :wqa!<cr>
nnoremap <leader>s :sp<CR>
nnoremap <leader><leader>s :setlocal spell!<CR>
nnoremap <leader>r :so $MYVIMRC<CR>
nnoremap <leader>T :Windows<CR>
nnoremap <leader>l :Lines<CR>
nnoremap <leader>v :vs<CR>
" Fast saving
nnoremap <leader>w :w<cr>
" dont delete useful while searching
nnoremap <leader>zz :let &scrolloff=999-&scrolloff<CR>
" }}} leader maping end
" Clipboard madness {{{
" replace currently selected text with default register
" without yanking it
vnoremap <leader>p "_dP
" paste from the primary clipboard/ selection
nnoremap <leader>P "*p
" paste from the secondary clipboard/ ctrl+c
nnoremap <leader><leader>p "+p
" yank from the primary clipboard/ selection
nnoremap <leader>Y "*y
" yank from the secondary clipboard/ ctrl+c
nnoremap <leader>y "+y
" }}} clipboard madness
" }}}1 Shortcut Mappings
" Filetype Specific Settings {{{
augroup FTOptions
    autocmd!
    autocmd filetype xml,xsd,html,javascript,yaml setlocal shiftwidth=2 softtabstop=2 tabstop=2 nostartofline
    autocmd FileType xdefaults                    setlocal commentstring=!\ %s
    autocmd filetype c,cpp,java,go                setlocal foldmethod=syntax foldlevel=99 complete-=k
    autocmd FileType liquid,text,txt,tex          setlocal complete+=k textwidth=80
    autocmd filetype vim                          setlocal foldmethod=marker keywordprg=:help
    autocmd filetype sh                           setlocal keywordprg=man shiftwidth=2
    autocmd filetype xml,sh,vim,tex,html,lua      setlocal foldmethod=marker foldlevel=99
    autocmd Filetype gitcommit                    setlocal spell textwidth=72
    autocmd FileType git,gitcommit                setlocal foldmethod=syntax
augroup end
"}}} Filetype Specific Settings
" Plugin Specific Settings ====================================================
if executable('rg')
      let g:ackprg = 'rg --vimgrep'
endif
let g:ack_autoclose = 0
let g:ViewDoc_DEFAULT = 'ViewDoc_help'
" Plugin Specific Settings ================================================
if filereadable(glob("~/.vimrc.local"))
    source ~/.vimrc.local
endif
augroup numbertoggle
    autocmd!
    autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &nu | set rnu   | endif
    autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &nu | set nornu | endif
augroup END

" For compatability with tmux
" Using Meta-[hjkl] mappings in tmux to move panes
if has('mac')
    nnoremap <silent> ˙ :TmuxNavigateLeft<cr>
    nnoremap <silent> ∆ :TmuxNavigateDown<cr>
    nnoremap <silent> ˚ :TmuxNavigateUp<cr>
    nnoremap <silent> ¬ :TmuxNavigateRight<cr>
endif

nmap ]h <Plug>GitGutterNextHunk
nmap [h <Plug>GitGutterPrevHunk

let g:rooter_silent_chdir = 1 " airblade.vim-rooter.settings
let g:rooter_change_directory_for_non_project_files = 'current' " airblade.vim-rooter.settings

command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)

" Select text for which we need boxes drawn
" https://github.com/ascii-boxes/boxes
vmap <leader>db !boxes -d stone -p v1 -a hc -s 80
vmap <leader>xc !boxes -r<CR>
vmap y ygv<Esc>|" After yanking in visual mode move cursor to the end of  the selection

let g:lightline = {
      \ 'component_function': {
      \   'filename': 'LightLineFilename'
      \ }
      \ }
function! LightLineFilename()
  " Get shrinked current working directory and filename
  return  substitute(getcwd(), '\(/.\)\([^/]*\)' , "\\1", "g") . ' | ' . expand('%')
endfunction
" Repeat last command in the next tmux pane.
" https://ricostacruz.com/til/repeat-tmux-from-vim
function! s:TmuxRepeat()
  silent! exec "!tmux select-pane -l && tmux send up enter && tmux select-pane -l"
  redraw!
endfunction
noremap  <leader>j :w<CR>:call <SID>TmuxRepeat()<CR>
" How to write comments on the same line as map in vimrc: https://stackoverflow.com/a/24717020
" :h map-comments
" :h map-bar
