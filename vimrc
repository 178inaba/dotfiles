if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
Plug 'fatih/vim-go'
Plug 'Shougo/neocomplete.vim'
call plug#end()

" Basics
set whichwrap=h,l

" vim-go
let g:go_fmt_command = "goimports"

" neocomplete.vim
let g:neocomplete#enable_at_startup = 1
