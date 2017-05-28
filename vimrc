if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
Plug 'airblade/vim-gitgutter'
Plug 'altercation/vim-colors-solarized'
Plug 'editorconfig/editorconfig-vim'
Plug 'fatih/vim-go'
Plug 'kchmck/vim-coffee-script'
Plug 'mattn/sonictemplate-vim'
Plug 'Shougo/neocomplete.vim'
Plug 'tpope/vim-fugitive'
Plug 'udalov/kotlin-vim'
call plug#end()

" Basics
set backspace=indent,eol,start
set directory=~/tmp,/var/tmp,/tmp
set hlsearch
set ruler
set whichwrap=b,s,h,l,<,>,~,[,]

" Indents
set shiftwidth=4
set tabstop=4

" altercation/vim-colors-solarized
set background=dark
let g:solarized_termcolors = 256
colorscheme solarized

" fatih/vim-go
let g:go_fmt_command = "goimports"
let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_generate_tags = 1

" Shougo/neocomplete.vim
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_auto_select = 1
