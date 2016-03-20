set encoding=utf-8
scriptencoding utf8

nnoremap <Space>. :edit $MYVIMRC<CR>

" :ReloadVimrc load $MYVIMRC
command! ReloadVimrc source $MYVIMRC

augroup vimrc
  autocmd!
augroup END

"" Plugins

let s:dein_dir = expand('~/.cache/dein')
let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo/dein.vim'

let s:dein_toml = expand('~/.vim/dein.toml')
let s:dein_lazy_toml = expand('~/.vim/dein_lazy.toml')

if has('vim_starting')
  execute 'set runtimepath+=' . s:dein_repo_dir
  if !isdirectory(s:dein_repo_dir)
    echo 'install vim-plug...'
    call mkdir(s:dein_repo_dir, 'p')
    call system('git clone https://github.com/Shougo/dein.vim ' . s:dein_repo_dir)
  end
endif

if dein#load_state(s:dein_dir)
  call dein#begin(s:dein_dir, [expand('<sfile>'), s:dein_toml, s:dein_lazy_toml]) "{{{

  call dein#load_toml(s:dein_toml, {'lazy': 0})
  call dein#load_toml(s:dein_lazy_toml, {'lazy': 1})

  call dein#end() "}}}
  call dein#save_state()
endif

if has('vim_starting') && dein#check_install()
    call dein#install()
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" language support
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

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
" nnoremap ; :
" nnoremap : ;
" vnoremap ; :
" vnoremap : ;

