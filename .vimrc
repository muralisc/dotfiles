" vim: foldlevel=99:
"Courtsey
"Vincent Driessen <vincent@datafox.nl> http://nvie.com/posts/how-i-boosted-my-vim/
"Tsung-Hsiang (Sean) Chang <vgod@vgod.tw>
"and Vim User Manual
"The commands are are anged in the order encountered in vim user manual

set nocompatible                                                                " not compatible with the old-fashion vi mode
" vim-plug setup {{{
if filereadable(expand("~/.vim/autoload/plug.vim"))
  call plug#begin('~/.vim/plugged')
  " Plug 'mattn/emmet-vim'                                                      "Use while coding html
  Plug 'christoomey/vim-tmux-navigator'
  " Plug 'SirVer/ultisnips'                                                       " for snippets DocuHB
  Plug 'muralisc/vim-colorschemes'
  Plug 'kien/ctrlp.vim'
  Plug 'mileszs/ack.vim'
  Plug 'octol/vim-cpp-enhanced-highlight'                                       " highlighting for STL
  Plug 'powerman/vim-plugin-viewdoc'
  Plug 'tpope/vim-commentary'                                                   " map: gcc
  Plug 'tpope/vim-fugitive'                                                     " GIT
  Plug 'tpope/vim-surround'                                                     " map: ys{tobj}[>)}] - for no space
  Plug 'tpope/vim-unimpaired'                                                   " shorcut for various toggles
  Plug 'muralisc/vim-snippets'
  Plug 'godlygeek/tabular'
  Plug 'vim-scripts/restore_view.vim'
  " Plug 'ap/vim-buftabline'                                                    " uncomment when required
  " Plug 'scrooloose/nerdtree'                                                  " uncomment when required
  Plug 'vim-airline/vim-airline'
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'junegunn/fzf.vim'                                                       " :Colors :Lines and shell **
  call plug#end()
endif
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
set vb t_vb=                                                                    " prevent screen flasing on multiple esc
set t_Co=256                                                                    " set 256 colors in vim
set background=dark
let mapleader="\<Space>"                                                        " Change the mapleader from \ to
set autoread                                                                    " read a changed file on disk
set showmode                                                                    " always show what mode we're currently editing in
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
" }}}
set shiftround                                                                  " use multiple of shiftwidth when indenting with '<' and '>'
set backspace=indent,eol,start                                                  " allow backspacing over everything in insert mode
set ignorecase                                                                  " ignore case when searching
" Visual {{{
set showmatch                                                                   " set show matching parenthesis
set number                                                                      " always show line numbers
set rnu                                                                         " relative number
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
set textwidth=170        " not 80 cause helps in vs mode
" Ease of Use {{{ "
set wildmenu                                                                    " tab completion for files/buffers like bash
set wildmode=list:full                                                          " show a list when pressing tab and complete first full match
set wildignore=*.swp,*.bak,*.pyc,*.class
set wildignore+=*/node_modules/*
set wildignorecase                                                              " ignore case while filename complete
set title                                                                       " change the terminal's title
set visualbell                                                                  " don't beep
set noerrorbells                                                                " don't beep
set showcmd                                                                     " show (partial) command at right bottom; this also shows visual selection info
" }}} Ease of Use "
set modeline                                                                    " enable mode lines
set ttyfast                                                                     " always use a fast terminal
set spell spelllang=en_us
set nospell
set colorcolumn=80,135                                                          " Github limit
set diffopt+=vertical                                                           " default split method is to split in a verical split
set dictionary=/usr/share/dict/cracklib-small
set tags=tags;~,my-tags;~                                                       "seach for tags|TAGS|my-tags and bubble up till home direcotry
set viewoptions-=options                                                        " to make restore_view work well
silent! colorscheme neverland-darker
"}}} Basic Settings
" Folding Rules {{{
set foldenable                                                                  " enable folding
set foldcolumn=0                                                                " add a fold column
set foldmethod=marker                                                           " detect triple-{ style fold markers [marker indent]
set foldlevel=99                                                                " 0-foldall 99-unfoldall
set foldopen=block,hor,insert,jump,mark,percent,quickfix,search,tag,undo        " which commands trigger auto-unfold

"Courtsey http://inlehmansterms.net/2014/09/04/sane-vim-working-directories/
function! SetProjectRoot()
  " default to the current file's directory
  lcd %:p:h
  let git_dir = system("git rev-parse --show-toplevel")
  " See if the command output starts with 'fatal' (if it does, not in a git repo)
  let is_not_git_dir = matchstr(git_dir, '^fatal:.*')
  " if git project, change local directory to git project root
  if empty(is_not_git_dir)
    lcd `=git_dir`
  endif
endfunction
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
set cmdheight=1                                                                 " use a status bar that is 2 rows high
" set statusline=
" set statusline+=%7*\[%n]                                  "buffernr
" set statusline+=%1*\ %<%F\                                "File+path
" set statusline+=%2*\ %y\                                  "FileType
" set statusline+=%3*\ %{''.(&fenc!=''?&fenc:&enc).''}      "Encoding
" set statusline+=%3*\ %{(&bomb?\",BOM\":\"\")}\            "Encoding2
" set statusline+=%4*\ %{&ff}\                              "FileFormat (dos/unix..)
" set statusline+=%5*\ asdf\  "Spellanguage & Highlight on?
" set statusline+=%8*\ %=\ row:%l/%L\ (%03p%%)\             "Rownumber/total (%)
" set statusline+=%9*\ col:%03c\                            "Colnr
" set statusline+=%0*\ \ %m%r%w\ %P\ \                      "Modified? Readonly? Top/bot.
" }}} Editor Layout
" Shortcut Mappings {{{
" resize
nnoremap <Up>    5<c-w>+
nnoremap <Down>  5<c-w>-
nnoremap <Right> 5<c-w>>
nnoremap <Left>  5<c-w><
" search seected text
vnoremap // y/<C-R>"<CR>
" insert mode {{{
" inoremap <c-e> <C-o>A ------  use o
" Dont move your fingers from the home row OR use ctrl-[ instead
inoremap jj <Esc>
" }}} insert mode
" normal mappings {{{
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
" instead of scorlling to middle .. scroll almost to top
nnoremap zz zt5<C-y>
" }}}  normal mappings
" leader mapings {{{
" home row {{{
nnoremap <Leader>a zA
" s for structure
nnoremap <Leader>du :diffupdate<CR>
nnoremap <Leader>gl :silent! Glog --<CR>
" when browsing glog, when you are in diff and want to go back to commit TODO
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <leader>gs :Gstatus<CR>
" ge is used after n_CTRL-e
nnoremap <Leader>ge :GitGutterNextHunk<CR>
" ge is used after n_CTRL-y
nnoremap <Leader>gy :GitGutterPrevHunk<CR>
" }}}

" Clears the search register
nnoremap <leader>/ :nohlsearch<CR>
nnoremap <leader>f :CtrlPBuffer<CR>
" with vimgrep, see results in cope(leader+cc) next (]q) previous ([q)
nnoremap <leader>co :botright cope<cr>
nnoremap <leader>cc :cclose<cr>
" Switch CWD to the directory of the open buffer
nnoremap <leader>cd :cd %:p:h<cr>:pwd<cr>
nnoremap <leader>dc :call SetProjectRoot()<cr>
" make file ( use quick fix window to see errors )
nnoremap <leader>m :cd %:p:h<cr>:pwd<cr>:!clear<CR>:w<CR>:make<CR>
" NERD
nnoremap <leader>n :NERDTreeFind<CR>
nnoremap <leader>N :NERDTreeToggle<CR>

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
nnoremap <leader>q :bp\|bd #<cr>
" Close splits but not last window
nnoremap <leader><leader>q :close!<cr>
" Close vim itself
nnoremap <leader><leader><leader>q :wqa!<cr>
" Open a shell in current directory
nnoremap <leader>s :shell<CR>
nnoremap <leader><tab> :q<cr>
nnoremap <leader>r :so $MYVIMRC<CR>
nnoremap <leader>t :CtrlPTag<CR>
" Useful mappings for managing tabs
nnoremap <leader>T :tabnew<cr>
nnoremap <leader>l :Lines<CR>
" Open vimgrep and put the cursor in the right position
nnoremap <leader>v :Ack! --ignore 'tags' <C-r><C-w>
" Fast saving
nnoremap <leader>w :w<cr>
" make the current file executable
nnoremap <leader>x :close<CR>
" dont delete useful while searching
nnoremap <Leader>zz :let &scrolloff=999-&scrolloff<CR>
" }}} leader maping end
" }}} Shortcut Mappings
" Filetype Specific Settings {{{
                                                                                " " Restore cursor position upon reopening files
" autocmd BufReadPost *
    " \ if line("'\"") > 0 && line("'\"") <= line("$") |
    " \   exe "normal! g`\"" |
    " \ endif
                                                                                 " Strip all trailing whitespace from a file
augroup FTOptions
    autocmd!
    autocmd filetype xml,xsd,html,javascript,yaml setlocal shiftwidth=2 softtabstop=2 tabstop=2 nostartofline
    autocmd FileType xdefaults                    setlocal commentstring=!\ %s
    autocmd filetype c,cpp,java,go                setlocal foldmethod=syntax foldlevel=99 complete-=k
    autocmd FileType liquid,markdown,text,txt     setlocal complete+=k
    autocmd filetype vim                          setlocal keywordprg=:help
    autocmd filetype sh                           setlocal keywordprg=man shiftwidth=2
    autocmd filetype xml,sh,vim,tex,html,lua      setlocal foldmethod=marker
    autocmd FileType gitcommit                    setlocal spell
    autocmd FileType git,gitcommit                setlocal foldmethod=syntax
augroup end
"}}} Filetype Specific Settings
" Plugin Specific Settings {{{
    " ACK.vim {{{
if executable('ag')
      let g:ackprg = 'ag --nogroup --nocolor --column'
endif
" }}}
    " viewdoc settings "{{{
let g:ViewDoc_DEFAULT = 'ViewDoc_help'
" }}}
    " YOU COMPLETE ME {{{
let g:ycm_confirm_extra_conf = 0
let g:ycm_always_populate_location_list = 1 "}}}
    " PYTHON MODE SETTINGS {{{
let g:pymode_rope = 1
" PYMODE ignore errors list
let g:pymode_lint_ignore = "E702,E501,E225,E221,E203,E231,E201,E202,E261,E262" "}}}
    " ctrlp settings{{{
let g:ctrlp_cmd = 'CtrlPMRUFiles' "}}}
    " VIM-FIGITIVE {{{
" close all unwanted buffers opened by vim fugitive (git blame)
" autocmd BufReadPost fugitive://* set bufhidden=delete
" }}}
    " delimitMate settings {{{
let delimitMate_expand_space = 1
let delimitMate_expand_cr = 1 "}}}
    " ULTISNIPS {{{
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-tab>"
let g:UltiSnipsEditSplit="vertical" "}}}
    " plasticboy markdown {{{
let g:vim_markdown_folding_disabled = 1
" }}}
    " airline {{{
let g:airline_right_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_left_alt_sep= ''
let g:airline_left_sep = ''
let g:airline_section_z = '%P %l:%c'
let g:airline#extensions#whitespace#checks = [ 'indent', 'trailing', 'long' ]
" }}}
"
" }}} Plugin Specific Settings
if filereadable(glob("~/.vimrc.local"))
    source ~/.vimrc.local
endif
