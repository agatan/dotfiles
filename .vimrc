set encoding=utf-8
scriptencoding utf8

nnoremap <Space>. :edit $MYVIMRC<CR>

" :ReloadVimrc load $MYVIMRC
command! ReloadVimrc source $MYVIMRC

" ================================================================================
" minpac {{{

packadd minpac

if !exists('*minpac#init')
  " minpac not found.
  throw 'minpac not found. Run `git clone https://github.com/k-takata/minpac.git ~/.vim/pack/minpac/opt/minpac`.'
else
  call minpac#init()

  call minpac#add('cocopon/iceberg.vim')
  call minpac#add('itchyny/lightline.vim')
  let g:lightline = {
      \ 'colorscheme': 'powerline',
      \   'active': {
      \     'left': [ [ 'mode', 'paste' ],
      \               ['readonly', 'filename', 'modified' ] ]
      \   },
      \ }

  call minpac#add('editorconfig/editorconfig-vim')

  call minpac#add('junegunn/fzf')
  call minpac#add('junegunn/fzf.vim')
  nnoremap [fzf] <Nop>
  nmap <Space>f [fzf]
  nnoremap <silent> [fzf]f :<C-u>Files<CR>
  nnoremap <silent> [fzf]m :<C-u>History<CR>
  nnoremap <silent> [fzf]b :<C-u>Buffers<CR>

  call minpac#add('itchyny/vim-cursorword')

  call minpac#add('thinca/vim-qfreplace')

  call minpac#add('prabirshrestha/async.vim')
  call minpac#add('prabirshrestha/asyncomplete.vim')
  call minpac#add('prabirshrestha/asyncomplete-lsp.vim')
  call minpac#add('prabirshrestha/vim-lsp')
  call minpac#add('mattn/vim-lsp-settings')
  call minpac#add('hrsh7th/vim-vsnip')
  call minpac#add('hrsh7th/vim-vsnip-integ')
  function! s:on_lsp_buffer_enabled() abort
    setlocal omnifunc=lsp#complete
    setlocal signcolumn=yes
    nmap <buffer> gd <plug>(lsp-definition)
    nmap <buffer> <f2> <plug>(lsp-rename)
    nmap <buffer> fmt <plug>(lsp-document-format)
    nmap <buffer> K <plug>(lsp-hover)
    inoremap <expr> <cr> pumvisible() ? "\<c-y>\<cr>" : "\<cr>"
  endfunction

  augroup lsp_install
    au!
    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
  augroup END
  command! LspDebug let lsp_log_verbose=1 | let lsp_log_file = expand('~/lsp.log')

  let g:lsp_diagnostics_enabled = 1
  let g:lsp_diagnostics_echo_cursor = 1
  let g:asyncomplete_auto_popup = 1
  let g:asyncomplete_auto_completeopt = 0
  let g:asyncomplete_popup_delay = 200
  let g:lsp_text_edit_enabled = 1
  let g:lsp_settings = {
        \   'pyls': {
        \     'workspace_config': {
        \         'plugins': {
        \           'pyls_mypy': { 'enabled': v:true, 'live_mode': v:false }
        \         }
        \     }
        \   }
        \ }

  call minpac#add('tyru/caw.vim')
  nmap <Space>c <Plug>(caw:hatpos:toggle)
  vmap <Space>c <Plug>(caw:hatpos:toggle)

  call minpac#add('junegunn/vim-easy-align')

  call minpac#add('tpope/vim-endwise')
  call minpac#add('tpope/vim-surround')

  call minpac#add('cespare/vim-toml')
  call minpac#add('mrk21/yaml-vim')
end

command! PackUpdate packadd minpac | source $MYVIMRC | call minpac#update('', {'do': 'call minpac#status()'})
command! PackClean  packadd minpac | source $MYVIMRC | call minpac#clean()
command! PackStatus packadd minpac | source $MYVIMRC | call minpac#status()

" }}}


" ================================================================================
" Language {{{
augroup myvimrc
  au!
  autocmd FileType ruby setlocal regexpengine=1
  autocmd FileType crystal setlocal regexpengine=1
augroup END
" }}}


" ================================================================================
" Vim builtin settings {{{

filetype plugin indent on
syntax enable

set background=dark
colorscheme iceberg

set incsearch
set ignorecase
set smartcase
if !exists('loaded_matchit')
  " matchitを有効化
  runtime macros/matchit.vim
endif

nnoremap <C-]> g<C-]>
augroup grepquickfix
  autocmd!
  autocmd QuickFixCmdPost *grep* cwindow
augroup END

let g:mapleader = ','
set clipboard=unnamed,unnamedplus
set hidden
set noshowmode
set signcolumn=yes
set scrolloff=5
set cmdheight=2
set breakindent
set ruler
set laststatus=2
set foldmethod=marker
set foldlevel=100
set list
set listchars=tab:▸\ ,trail:-,eol:¬
set t_vb=
set visualbell
set noerrorbells
set backspace=indent,eol,start
set autoindent
set shiftwidth=4
set tabstop=4
set softtabstop=4
set expandtab
set showmatch
set matchtime=1
set whichwrap=b,s,h,l,<,>,[,]
set smarttab
set completeopt=noinsert,noselect,menuone

inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr>    pumvisible() ? "\<C-y>" : "\<cr>"

nnoremap ; :
vnoremap ; :
nnoremap : ;
vnoremap : ;
nnoremap q; q:
vnoremap q; q:

" }}}
