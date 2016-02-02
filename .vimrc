set encoding=utf-8
scriptencoding utf8

nnoremap <Space>. :edit $MYVIMRC<CR>

" :ReloadVimrc load $MYVIMRC
command! ReloadVimrc source $MYVIMRC

augroup vimrc
  autocmd!
augroup END

"" Plugins

if has('vim_starting')
  set rtp+=~/.vim/plugged/vim-plug
  if !isdirectory(expand('~/.vim/plugged/vim-plug'))
    echo 'install vim-plug...'
    call system('mkdir -p ~/.vim/plugged/vim-plug')
    call system('git clone https://github.com/junegunn/vim-plug.git ~/.vim/plugged/vim-plug/autoload')
  end
endif

call plug#begin('~/.vim/plugged')
  Plug 'junegunn/vim-plug',
        \ {'dir': '~/.vim/plugged/vim-plug/autoload'}

  "" async
  Plug 'Shougo/vimproc.vim', { 'do' : 'make' }

  "" work tracker
  Plug 'wakatime/vim-wakatime'

  "" Colorschemes
  Plug 'cocopon/iceberg.vim'

  "" navigation
  Plug 'ctrlpvim/ctrlp.vim' " integration navigation selection method
  Plug 'sgur/ctrlp-extensions.vim' " extensions for ctrlp plugin
  Plug 'airblade/vim-rooter' " change directory to project root
  Plug 'Lokaltog/vim-easymotion' " easy cursor moving
  Plug 'lambdalisue/vim-gita', { 'on' : ['Gita'] } " git integration

  "" ui
  Plug 'airblade/vim-gitgutter' " show git diff
  Plug 'itchyny/lightline.vim' " cool status line

  "" coding support
  Plug 'tyru/caw.vim' " comment lines
  Plug 'junegunn/vim-easy-align', { 'on' : ['EasyAlign'] } " alignment code
  Plug 'tpope/vim-surround' " surrounding code blocks

  "" auto complete
  Plug 'Shougo/neocomplete'
  Plug 'Shougo/neosnippet'
  Plug 'Shougo/neosnippet-snippets'

  "" syntax checker
  Plug 'scrooloose/syntastic'


  "" language support
  
  " c/c++
  Plug 'osyo-manga/vim-stargate', { 'for' : ['c', 'cpp'] } " smart #include
  Plug 'justmao945/vim-clang', { 'for' : ['c', 'cpp'] } " clang integration
  Plug 'agatan/vim-sort-include', { 'for' : ['c', 'cpp'] } " sort #include statements

  " dlang
  Plug 'idanarye/vim-dutyl', { 'for' : ['d'] }

  " golang
  Plug 'fatih/vim-go', { 'for' : ['go'] } " all of go support

  " vim script
  Plug 'rbtnn/vimconsole.vim', { 'on' : ['VimConsoleOpen'] } " REPL for vimscript
  Plug 'thinca/vim-prettyprint', { 'on' : ['PrettyPrint'] } " prrety print for vimscript object
call plug#end()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" plugin settings (exclude language plugins)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" ctrlp
let g:ctrlp_extensions = ['cmdline', 'menu']

" easy-motion
nmap mm <Plug>(easymotion-s2)

" vim-rooter
let g:rooter_use_lcd = 1

" caw.vim
nnoremap <Space>c <Plug>(caw:I:toggle)
vnoremap <Space>c <Plug>(caw:I:toggle)
nnoremap <Space>C <Plug>(caw:I:uncomment)
vnoremap <Space>C <Plug>(caw:I:uncomment)

" neocomplete
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return (pumvisible() ? "\<C-y>" : "") . "\<CR>"
endfunction
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
if !exists('g:neocomplete#sources#omni#input_patterns')
  let g:neocomplete#sources#omni#input_patterns = {}
endif

" neosnippet
imap <C-k> <Plug>(neosnippet_expand_or_jump)
smap <C-k> <Plug>(neosnippet_expand_or_jump)
xmap <C-k> <Plug>(neosnippet_expand_or_jump)

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" language support
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"" c/c++
" include path
augroup cpp-path
  autocmd!
  autocmd FileType cpp execute 'setlocal path=.,/usr/include,/usr/local/include' .
        \ join(filter(split(glob('/usr/include/**/c++/*'), '\n'),
        \            'isdirectory(v:val)'),
        \      ',')
augroup END
" indent option
set cinoptions+=:0,g0
" vim-clang settings
let g:clang_compilation_database = './build'
let g:clang_cpp_complete_opt = 'menuone,preview,noinsert'
let g:clang_auto = 0
let g:clang_cpp_options = '-std=c++1z'
let g:clang_format_auto = 1
let g:clang_format_style = 'LLVM'
let g:clang_check_syntax_auto = 1
" sort-include settings
augroup sort-include
  autocmd!
  autocmd BufWritePre *.{c,cpp,h,hpp} SortInclude
augroup END

"" dlang
augroup dlang
  autocmd!
  autocmd FileType d setlocal ts=4 sw=4 softtabstop=0
augroup END

"" golang
let g:go_fmt_command = "goimports"
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:syntastic_go_checkers = ['go', 'govet', 'golint']
let g:neocomplete#sources#omni#input_patterns.go = '\h\w\.\w*'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" foundamental settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""

"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 全般
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
set noswapfile
set nobackup
set clipboard=unnamed,unnamedplus
set t_vb=
set visualbell
set noerrorbells

"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" display
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
if !has('gui_running')
    set t_Co=256
endif
colorscheme iceberg

set hidden
set number
set scrolloff=5
set wrap
set showbreak=+
set cursorline
set cursorcolumn
set colorcolumn=80
set ruler
set tabstop=2
set softtabstop=2
set expandtab
set smarttab
set laststatus=2
set foldlevel=100
set list
set listchars=tab:>_,trail:-,eol:$
" 補完ポップアップの高さ設定
set pumheight=10
syntax on

augroup vimrc
    autocmd BufWritePost * mkview
    autocmd BufReadPost * loadview
augroup END

"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" search
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
set incsearch
set ignorecase
set smartcase
set nohlsearch
set keywordprg=:help
if !exists('loaded_matchit')
  " matchitを有効化
  runtime macros/matchit.vim
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" input
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
set backspace=indent,eol,start
set autoindent
set shiftwidth=2
set showmatch
set whichwrap=b,s,h,l,<,>,[,]
set smarttab


" 英字配列だとコロンがつらい
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;



