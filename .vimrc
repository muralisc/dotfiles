" vim: foldlevel=2:
" Irrelevent sections are given a foldlevel of 3 so that they are folded by default
" Courtsey :
" Vincent Driessen <vincent@datafox.nl> http://nvie.com/posts/how-i-boosted-my-vim/
" Tsung-Hsiang (Sean) Chang <vgod@vgod.tw>
" junegunn: https://github.com/junegunn/dotfiles/blob/master/vimrc
" https://github.com/yoshuawuyts/dotfiles
" and Vim User Manual
let g:loaded_matchparen = 1
set nocompatible                                                     " not compatible with the old-fashion vi mode
" vim-plug setup {{{1
if filereadable(expand("~/.vim/autoload/plug.vim"))
  call plug#begin('~/.vim/plugged')
  " Plug 'tpope/vim-vinegar'                                         " Folder navigation ? C u r cd CD                   
  Plug 'dense-analysis/ale'                                          " Async Syntax checking (with cpp, rust,shellcheck) 
  let g:ale_cpp_clang_options = '-std=c++17 -Wall'
  let g:ale_cpp_gcc_options = '-Wall -O2 -std=c++1z'
  let g:ale_linters = {'c': ['clang'], 'cpp': ['clang', 'gcc'],'go': ['golangci-lint', 'gofmt', 'go vet']}
  let g:ale_lint_on_text_changed = 'normal'
  let g:ale_lint_on_insert_leave = 1
  let g:ale_lint_on_enter = 0
  Plug 'mileszs/ack.vim'                                             " Search files                                      
  Plug 'powerman/vim-plugin-viewdoc'                                 " For viewing help files                            
  Plug 'tpope/vim-commentary'                                        " map: gcc                                          
  Plug 'tpope/vim-surround'                                          " map: ys[ <{( >)} ] - for no space                
  Plug 'tpope/vim-unimpaired'                                        " yon | yor | yow | ]q | [q |                       
  Plug 'tpope/vim-fugitive'
  Plug 'godlygeek/tabular'                                           " for easily aligning                               
  Plug 'vim-scripts/restore_view.vim'                                "                                                   
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'junegunn/fzf.vim'
  command! -bang -nargs=* Rg
        \ call fzf#vim#grep(
        \   'rg --sortr path --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
        \   <bang>0 ? fzf#vim#with_preview('up:60%')
        \           : fzf#vim#with_preview('right:50%:hidden', '?'),
        \   <bang>0)
  Plug 'christoomey/vim-tmux-navigator'
  Plug 'itchyny/lightline.vim'
  Plug 'airblade/vim-rooter'
  let g:rooter_silent_chdir = 1 " airblade.vim-rooter.settings
  let g:rooter_change_directory_for_non_project_files = 'current' " airblade.vim-rooter.settings
  let g:rooter_patterns = []
  Plug 'SirVer/ultisnips'
  let g:UltiSnipsExpandTrigger="<tab>"
  let g:UltiSnipsListSnippets="<c-tab>"
  Plug 'honza/vim-snippets'
  Plug 'muralisc/snippets'
  Plug 'bfredl/nvim-miniyank'
  Plug 'whiteinge/diffconflicts'
  "ORG MODE and Helpers
  Plug 'mattn/calendar-vim'
  Plug 'tpope/vim-speeddating'
  Plug 'jceb/vim-orgmode'
  " Language Specific
  Plug 'fatih/vim-go'
  " Non-essential
  Plug 'junegunn/rainbow_parentheses.vim'
  Plug 'tpope/vim-vinegar'
  " Colorschemes
  Plug 'morhetz/gruvbox'
  let g:gruvbox_contrast_dark="hard"
  Plug 'joshdick/onedark.vim'
  Plug 'dracula/vim'
  Plug 'chriskempson/base16-vim'
  Plug 'whatyouhide/vim-gotham'
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
"Credit joshdick
"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (empty($TMUX))
  if (has("nvim"))
  "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  endif
  "For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
  "Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
  " < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
  if (has("termguicolors"))
    set termguicolors
  endif
else
  if (has("termguicolors"))
    "Set Vim-specific sequences for RGB colors; only seems to be needed for Vim 8 running inside tmux with $TERM=tmux
    "Found at < https://github.com/vim/vim/issues/993#issuecomment-255651605 >
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
  endif
endif

set background=dark
let mapleader="\<Space>"                                                        " Change the mapleader from \ to
let maplocalleader="\<Space>"
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
set viminfo='500,<80                                                            " read/write a .viminfo file, don't store more than 80 lines of registers
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
set colorcolumn=80,132                                                          " Github limit
set diffopt+=vertical                                                           " default split method is to split in a verical split
set dictionary=/usr/share/dict/cracklib-small
set tags=tags;~,my-tags;~                                                       " seach for tags|TAGS|my-tags and bubble up till home direcotry
set viewoptions-=options                                                        " to make restore_view work well
silent! colorscheme gotham                      " gruvbox onedark dracula gotham base16-*
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
" Thanks to Steve Losh for this liberating tip[perl/python compatible regex]
" See http://stevelosh.com/blog/2010/09/coming-home-to-vim
nnoremap / /\v
vnoremap / /\v
" never go into Ex mode
nnoremap Q <nop>
nnoremap Y y$
" instead of scorlling to middle .. scroll almost to top
nnoremap zz zt5<C-y>
" }}}  normal mappings
" leader mapings {{{
" Clears the search register
nnoremap <leader>n :nohlsearch<CR>
" Find in files:
nnoremap <leader>/ :Rg!
nnoremap <leader><leader>/ :Rg!<C-R><C-W>
nnoremap <leader>y/ :Rg!<C-R>"
" FZF is faster than CtrlP for finding files in Directories
nnoremap <leader>pf :GFiles
" alternate for => :CtrlPMRUFiles <CR>
nnoremap <leader>fr :History <CR>
" open another file in same dir as current file
nnoremap <leader>ff :FZF! +s --tac <CR>
" Quit Files with leader + q
nnoremap <leader>q :bp\|bd #<cr>
" Close splits but not last window
nnoremap <leader><leader>q :close!<cr>
" Close vim itself
nnoremap <leader><leader><leader>q :wqa!<cr>
nnoremap <leader>s :sp<CR>
nnoremap <leader>r :so $MYVIMRC<CR>
nnoremap <leader>v :vs<CR>
" Fast saving
nnoremap <leader>w :w<cr>
" }}} leader maping end
" Clipboard madness {{{
map p <Plug>(miniyank-autoput)
map P <Plug>(miniyank-autoPut)
" replace currently selected text with default register
" without yanking it
vnoremap <leader>p "_dP
" paste from the primary clipboard/ selection
nnoremap <leader>P "*p
" paste from the secondary clipboard/ ctrl+c
nnoremap <leader><leader>p "+p
" yank to the primary clipboard/ selection
nnoremap <leader>Y "*y
" yank to the secondary clipboard/ ctrl+c
nnoremap <leader>y "+y
" }}} clipboard madness
" }}}1 Shortcut Mappings
" Filetype Specific Settings {{{
augroup FTOptions
  autocmd!
  autocmd filetype xml,xsd,html,javascript,yaml setlocal shiftwidth=2 softtabstop=2 tabstop=2 nostartofline
  autocmd FileType xdefaults                    setlocal commentstring=!\ %s
  autocmd filetype c,cpp,java,go                setlocal foldmethod=syntax foldlevel=99 complete-=k shiftwidth=2
  autocmd FileType liquid,text,txt,tex          setlocal complete+=k textwidth=80
  autocmd filetype vim                          setlocal foldmethod=marker keywordprg=:help shiftwidth=2
  autocmd filetype sh                           setlocal keywordprg=man shiftwidth=2
  autocmd filetype xml,sh,vim,tex,html,lua      setlocal foldmethod=marker foldlevel=99
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



" Select text for which we need boxes drawn
" https://github.com/ascii-boxes/boxes
" db - draw box
vnoremap <leader>db !boxes -d stone -p v1 -a hc -s 80
" xb - delete box
vnoremap <leader>xb !boxes -r<CR>
" Below is a method of writing comment in the same line as a map
vnoremap y ygv<Esc>|" After yanking in visual mode move cursor to the end of  the selection

let g:lightline = {
      \ 'colorscheme': 'dracula',
      \ 'component_function': {
      \   'filename': 'LightLineFilename'
      \ }
      \ }
function! LightLineFilename()
  " Get shrinked current working directory and filename
  return  substitute(getcwd(), '\(/.\)\([^/]*\)' , "\\1", "g") . ' | ' . expand('%')
endfunction

" Delete file
nnoremap <F5> :call delete(expand('%')) <bar> bdelete! <CR>
