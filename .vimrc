" Always display file name
set modeline
set ls=2
" Line indicating where the cursor is
set cursorline
" Make sure 5 lines are always padding top or bottom
set scrolloff=10
" Line wraps
set showbreak=↪
" Syntax
syntax on
" Indent
set smartindent
" Always show line numbers, but only in current window.
set number
:au WinEnter * :setlocal number
:au WinLeave * :setlocal nonumber

" Needed for snipMate
:filetype plugin on

" Disable errorbell
:set noeb vb t_vb=

autocmd BufNewFile *.cc
  \ exe "normal O#include <iostream>\n#include <string>\n#include <cassert>\nusing namespace std;\n\nint main(int argc, const char* argv[]){\n\t\n}"
