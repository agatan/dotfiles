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
  Plug 'airblade/vim-rooter' " change directory to project root
  Plug 'Lokaltog/vim-easymotion' " easy cursor moving
  Plug 'lambdalisue/vim-gita', { 'on' : ['Gita'] } " git integration

  Plug 'Shougo/unite.vim' " integrated user interface
  Plug 'Shougo/neomru.vim' " Most Recently Used files
  Plug 'Shougo/unite-outline' " outliner
  Plug 'tsukkee/unite-tag' " tag file search
  Plug 'ujihisa/unite-colorscheme' " colorscheme previewer

  "" ui
  Plug 'airblade/vim-gitgutter' " show git diff
  Plug 'itchyny/lightline.vim' " cool status line

  "" coding support
  Plug 'tyru/caw.vim' " comment lines
  Plug 'junegunn/vim-easy-align', { 'on' : ['EasyAlign'] } " alignment code
  Plug 'tpope/vim-surround' " surrounding code blocks

  "" syntax checker
  Plug 'scrooloose/syntastic'


  "" language support
  
  " c/c++
  Plug 'osyo-manga/vim-stargate', { 'for' : ['c', 'cpp'] } " smart #include
  Plug 'justmao945/vim-clang', { 'for' : ['c', 'cpp'] } " clang integration

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

" unite
let g:unite_enable_start_insert = 1
let g:unite_enable_smart_case = 1
if (executable('ag'))
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_default_opts = '--nocolor --nogroup'
endif

" unite keymaps
nnoremap [unite] <Nop>
nmap <Space>u [unite]

nnoremap <silent> [unite]f :<C-u>Unite -start-insert file_rec file_mru file/new<CR>
nnoremap <silent> [unite]d :<C-u>Unite -start-insert directory_rec directory_mru directory/new<CR>
nnoremap <silent> [unite]b :<C-u>Unite -start-insert buffer<CR>
nnoremap <silent> / :<C-u>Unite -buffer-name=search line -start-insert<CR>
nnoremap <silent> [unite]r :<C-u>UniteResume<CR>

" easy-motion
nmap mm <Plug>(easymotion-s2)

" vim-rooter
let g:rooter_use_lcd = 1

" caw.vim
nmap <Space>c <Plug>(caw:I:toggle)
vmap <Space>c <Plug>(caw:I:toggle)
nmap <Space>C <Plug>(caw:I:uncomment)
vmap <Space>C <Plug>(caw:I:uncomment)

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
let g:syntastic_cpp_checkers = []

" let g:clang_check_syntax_auto = 1

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

"" OCaml
let s:opamshare = substitute(system('opam config var share'), '\n$', '', '''')
if isdirectory(s:opamshare)
  execute 'set rtp+=' . s:opamshare . '/merlin/vim'
  let g:syntastic_ocaml_checkers = ['merlin']
  execute 'set rtp+=' . s:opamshare . '/ocp-indent/vim'
  execute 'helptags ' . s:opamshare . '/merlin/vim/doc'
endif

function! s:ocaml_format()
  let now_line = line('.')
  execute ':%! ocp-indent'
  execute ':' . now_line
endfunction

function! s:ocaml_setup()
  nnoremap <Space>t :<C-u>MerlinTypeOf<CR>
  vnoremap <Space>t :<C-u>MerlinTypeOfSel<CR>
  nnoremap <Space>n :<C-u>MerlinGrowEnclosing<CR>
  nnoremap <Space>p :<C-u>MerlinShrinkEnclosing<CR>
  nnoremap <Space>r <Plug>(MerlinRename)
  nnoremap <Space>d :<C-u>MerlinDestruct<CR>
  nnoremap <Space>o :<C-u>MerlinOutline<CR>
  nnoremap <Space>gd :<C-u>MerlinLocate<CR>
endfunction

augroup ocaml
  autocmd!
  autocmd BufWritePre *.ml call s:ocaml_format()
  autocmd FileType ocaml call s:ocaml_setup()
augroup END

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
set foldmethod=marker
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



