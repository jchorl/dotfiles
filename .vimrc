"{{{Vundle
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
Plugin 'kien/ctrlp.vim'
Plugin 'Valloric/YouCompleteMe'
Plugin 'bling/vim-bufferline'
Plugin 'bling/vim-airline'
Plugin 'raimondi/delimitmate'

" Language specific plugins
Plugin 'groenewege/vim-less'
Plugin 'pangloss/vim-javascript'
Plugin 'fatih/vim-go'
Plugin 'nvie/vim-flake8'
Plugin 'hynek/vim-python-pep8-indent'

" All of your Plugins must be added before the following line
call vundle#end()            " required
"}}}

"{{{Auto Commands
" Automatically cd into the directory that the file is in
autocmd BufEnter * execute "chdir ".escape(expand("%:p:h"), ' ')

" Remove any trailing whitespace that is in the file
autocmd BufRead,BufWrite * if ! &bin | silent! %s/\s\+$//ge | endif
"}}}

"{{{General
"Colours
set t_Co=256

" Use the OS clipboard by default (on versions compiled with `+clipboard`)
set clipboard=unnamed

" Cool tab completion stuff
set wildmenu
set wildmode=list:longest,full

" Folding Stuffs
set foldmethod=marker

" Allow cursor keys in insert mode
set esckeys

" Allow backspace in insert mode
set backspace=indent,eol,start

" Optimize for fast terminal connections
set ttyfast

" Add the g flag to search/replace by default
set gdefault

" Don’t add empty newlines at the end of files
set binary
set noeol

set backupdir=~/.vim/backups
set directory=~/.vim/swaps
set undodir=~/.vim/undo

" Enable line numbers
set number
" Needed for Syntax Highlighting and stuff
filetype plugin indent on
syntax on
set grepprg=grep\ -nH\ $*

" Who doesn't like autoindent?
set autoindent

" Underline current line
set cursorline
hi clear CursorLine
hi clear LineNr
hi clear Folded
hi clear SpecialKey
hi CursorLine cterm=underline
hi Folded ctermbg=235
hi CursorLineNr ctermfg=33
hi bufferline_selected ctermfg=235 ctermbg=254

" Show “invisible” characters
set lcs=tab:→\ ,trail:·,eol:¬,nbsp:_
set list
" Highlight searches
set hlsearch
" Clearing highlighted searches
nmap <silent> // :nohlsearch<CR>
set ignorecase
set smartcase
" Highlight dynamically as pattern is typed
set incsearch
" Always show status line
set laststatus=2
" set statusline=%F%m%r%h%w\ {%Y}\ [%l,%v][%p%%]
" Disable error bells
set noerrorbells
" Don’t reset cursor to start of line when moving around.
set nostartofline
" Show the cursor position
set ruler
" Don’t show the intro message when starting Vim
set shortmess=atI
" Show the current mode
set showmode
" Show the filename in the window titlebar
set title
" Show the (partial) command as it’s being typed
set showcmd
" Spaces are better than a tab character
set expandtab
set smarttab
" Start scrolling three lines before the horizontal window border
set scrolloff=3

" This is totally awesome - remap jj to escape in insert mode.  You'll never type jj anyway, so it's great!
inoremap jj <Esc>

" Set to auto read when a file is changed from the outside
set autoread

" A buffer becomes hidden when it is abandoned
set hid

" Set leader to space bar
let mapleader = "\<Space>"

" leader o/b for ctrlp
nnoremap <Leader>o :CtrlP<CR>
nnoremap <Leader>b :CtrlPBuffer<CR>
nnoremap <Leader>d :bd<CR>
nnoremap <Leader>a :bufdo bd<CR>
" leader leader to write
nnoremap <Leader><Leader> :w<CR>
" leader q to wq
nnoremap <Leader>q :wq<CR>
" leader m to make
nnoremap <Leader>m :w <bar> make<CR>
" leader v for visual line
nnoremap <Leader>v V
" space to get out of visual mode
vnoremap <Space> <Esc>
" undo/redo with leader
nnoremap <Leader>u :undo<CR>
nnoremap <Leader>r :redo<CR>
" sudo write
nnoremap <leader>W :w !sudo tee % > /dev/null<CR>
" cycle buffers with leader n/p
nnoremap <leader>n :bn<CR>
nnoremap <leader>p :bp<CR>
" edit with leader e
nnoremap <leader>e :e 

" Go to end of pasted text
vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Move to end of line with C-e
inoremap <C-e> <Esc>A

" Paste while in insert mode with ctrl-v
inoremap <C-v> <C-R>*
"}}}

"{{{Window enter or exit
" Line indicating where the cursor is
:au WinEnter * :setlocal cursorline
:au WinLeave * :setlocal nocursorline
" Always show line numbers, but only in current window.
:au WinEnter * :setlocal number
:au WinLeave * :setlocal nonumber
"}}}

"{{{OS Specific
"Linux
set clipboard+=unnamedplus
"}}}

"{{{File specific
" http://tedlogan.com/techblog3.html
" spaces:
au FileType cpp,css,less,json,tex,yaml,html setlocal softtabstop=2 shiftwidth=2 expandtab
au FileType python,dockerfile setlocal softtabstop=4 shiftwidth=4 expandtab

" tabs:
au FileType java,javascript setlocal tabstop=4 softtabstop=4 shiftwidth=4 noexpandtab

" misc:
autocmd BufNewFile,BufRead *.json setfiletype json
autocmd BufNewFile,BufRead *.es6 setfiletype javascript

" Treat <li> and <p> tags like the block tags they are
let g:html_indent_tags = 'li\|p'

" compilation:
au FileType tex setlocal makeprg=pdflatex\ -shell-escape\ -interaction=nonstopmode\ -file-line-error\ *.tex

" Treat .md files as Markdown
autocmd BufNewFile,BufRead *.md setlocal filetype=markdown

" Fix line endings for windows
set ffs=unix,dos

" git commit messages
au FileType gitcommit setlocal spell textwidth=72

" right margin for python
au FileType python setlocal colorcolumn=80
"}}}

"{{{Plugins
" ctrlP
set runtimepath^=~/.vim/bundle/ctrlp.vim
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git'

" YouCompleteMe
" turn off the autocomplete preview window
set completeopt=longest,menuone
let g:ycm_path_to_python_interpreter="/usr/bin/python"

" bufferline
" dont show on the command line
let g:bufferline_echo = 0
" dont show numbers
let g:bufferline_show_bufnr = 0

let g:airline_powerline_fonts = 1

" delimitmate
" turn space expansion off in delimitmate
let delimitMate_expand_space = 0
" turn on smart quotes in delimitmate
let delimitMate_smart_quotes = 1
" turn on delimitmate carriage return expansion
let delimitMate_expand_cr = 1
" turns on delimitmate jumping over expansion
let delimitMate_jump_expansion = 1
" turn off delimitmate for certain filetypes
au FileType html let b:loaded_delimitMate = 1

" flake before write
au BufWritePost *.py call Flake8()
"}}}