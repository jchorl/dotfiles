""""""""""""""""""""""""""""""
" => Misc
""""""""""""""""""""""""""""""
set t_Co=256

inoremap jk <Esc>

set gdefault

set ignorecase
set smartcase

set number

set completeopt=menuone

" by default, the search highlight colour and confirm highlight colours are the same, so change the search highlight colour
highlight IncSearch guibg=DarkMagenta ctermbg=DarkMagenta term=underline

" use rg
map <leader>g :Ack! 
let g:ackprg = 'rg --vimgrep --no-heading'

set lcs=tab:→\ ,trail:·,eol:¬,nbsp:_
set list

set clipboard=unnamedplus

map <leader>q :wqa<cr>

""""""""""""""""""""""""""""""
" => Java section
""""""""""""""""""""""""""""""
au FileType java set autoindent
au FileType java set si
au FileType java set shiftwidth=2
"Java anonymous classes. Sometimes, you have to use them.
au FileType java set cinoptions+=j1

""""""""""""""""""""""""""""""
" => Ruby section
""""""""""""""""""""""""""""""
au FileType ruby set shiftwidth=2

""""""""""""""""""""""""""""""
" => CSS section
""""""""""""""""""""""""""""""
au FileType css set shiftwidth=2

""""""""""""""""""""""""""""""
" => js section
""""""""""""""""""""""""""""""
au FileType javascript set shiftwidth=2

""""""""""""""""""""""""""""""
" => Plugins
""""""""""""""""""""""""""""""
let g:jsx_ext_required = 0
let g:ycm_key_list_select_completion = ['<TAB>', '<Down>', '<C-j>']
