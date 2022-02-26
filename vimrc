" Install plug.
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Load settings for each location.
augroup vimrc-local
  autocmd!
  autocmd BufNewFile,BufReadPost * call s:vimrc_local(expand('<afile>:p:h'))
augroup END

function! s:vimrc_local(loc)
  let files = findfile('.vimrc.local', escape(a:loc, ' ') . ';', -1)
  for i in reverse(filter(files, 'filereadable(v:val)'))
    source `=i`
  endfor
endfunction

call plug#begin('~/.vim/plugged')
Plug 'airblade/vim-gitgutter'
Plug 'cespare/vim-toml'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'dart-lang/dart-vim-plugin'
Plug 'editorconfig/editorconfig-vim'
Plug 'fatih/vim-go'
Plug 'godlygeek/tabular'
Plug 'hashivim/vim-terraform'
Plug 'IN3D/vim-raml'
Plug 'JuliaEditorSupport/julia-vim'
Plug 'kchmck/vim-coffee-script'
Plug 'keith/swift.vim'
Plug 'leafgarland/typescript-vim'
Plug 'mattn/sonictemplate-vim'
Plug 'mattn/vim-lsp-settings'
Plug 'morhetz/gruvbox'
Plug 'natebosch/vim-lsc'
Plug 'posva/vim-vue'
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-buffer.vim'
Plug 'prabirshrestha/asyncomplete-file.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fugitive'
Plug 'udalov/kotlin-vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-syntastic/syntastic'
call plug#end()

" Basics
set backspace=indent,eol,start
set directory=~/tmp,/var/tmp,/tmp
set hlsearch
set laststatus=2
set noshowmode
set ruler
set termguicolors
set whichwrap=b,s,h,l,<,>,~,[,]
au BufNewFile,BufRead gitconfig setf gitconfig
au BufNewFile,BufRead *.tf,*.vue set expandtab
au BufWritePre *.php :%s/\s\+$//e
let g:PHP_vintage_case_default_indent = 1
autocmd FileType yaml setlocal sw=2 sts=2 ts=2 et

" Indents
set shiftwidth=4
set tabstop=4

" fatih/vim-go
let g:go_fmt_command = 'goimports'
let g:go_gocode_unimported_packages = 1
let g:go_highlight_array_whitespace_error = 1
let g:go_highlight_chan_whitespace_error = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_space_tab_error = 1
let g:go_highlight_trailing_whitespace_error = 1
let g:go_highlight_operators = 1
let g:go_highlight_functions = 1
let g:go_highlight_function_arguments = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_build_constraints = 1
let g:go_highlight_generate_tags = 1
let g:go_highlight_string_spellcheck = 1
let g:go_highlight_format_strings = 1
let g:go_highlight_variable_declarations = 1
let g:go_highlight_variable_assignments = 1
let g:syntastic_go_checkers = ['golint', 'govet']
let g:syntastic_mode_map = { 'mode': 'active', 'passive_filetypes': ['go'] }
let g:go_list_type = 'quickfix'
au BufRead,BufNewFile *.tmpl set ft=gohtmltmpl

" hashivim/vim-terraform
let g:terraform_align = 1
let g:terraform_fmt_on_save = 1

" morhetz/gruvbox
autocmd vimenter * ++nested colorscheme gruvbox

" prabirshrestha/asyncomplete.vim
let g:asyncomplete_auto_popup = 0
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1] =~ '\s'
endfunction

inoremap <silent><expr> <TAB>
  \ pumvisible() ? "\<C-n>" :
  \ <SID>check_back_space() ? "\<TAB>" :
  \ asyncomplete#force_refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

" prabirshrestha/asyncomplete-buffer.vim
call asyncomplete#register_source(asyncomplete#sources#buffer#get_source_options({
  \ 'name': 'buffer',
  \ 'whitelist': ['*'],
  \ 'completor': function('asyncomplete#sources#buffer#completor'),
  \ }))

" prabirshrestha/asyncomplete-file.vim
au User asyncomplete_setup call asyncomplete#register_source(asyncomplete#sources#file#get_source_options({
  \ 'name': 'file',
  \ 'whitelist': ['*'],
  \ 'priority': 10,
  \ 'completor': function('asyncomplete#sources#file#completor')
  \ }))

" prabirshrestha/asyncomplete-lsp.vim
let g:lsp_async_completion = 1

" https://github.com/prabirshrestha/asyncomplete.vim/issues/156#issuecomment-530170947
let g:lsp_text_edit_enabled = 0

" vim-syntastic/syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_wq = 0
