set encoding=utf-8
scriptencoding utf8

" tiny とsmall では読み込まない
if !1 | finish | endif

nnoremap <Space>. :edit $MYVIMRC<CR>

" :ReloadVimrc load $MYVIMRC
command! ReloadVimrc source $MYVIMRC

augroup myvimrc
  autocmd!
augroup END


"" Plugins

filetype off

let s:vim_plug_path = expand($HOME . '/.vim/plugged/')
if has('vim_starting')
    execute 'set runtimepath+=' . s:vim_plug_path . 'vim-plug'
    if !isdirectory(s:vim_plug_path)
        echo 'install vim-plug...'
        call mkdir(s:vim_plug_path, 'p')
        call system('git clone https://github.com/junegunn/vim-plug ' . s:vim_plug_path . 'vim-plug/autoload')
    end
endif
call plug#begin(s:vim_plug_path) " {{{

Plug 'Shougo/vimproc', { 'do': 'make' }
Plug 'vim-jp/vimdoc-ja'

" {{{ color
Plug 'rhysd/try-colorscheme.vim', { 'on': 'TryColorscheme' }
Plug 'cocopon/iceberg.vim'
Plug 'rhysd/wallaby.vim'
Plug 'w0ng/vim-hybrid'
Plug 'nanotech/jellybeans.vim'
Plug 'altercation/vim-colors-solarized'
Plug 'rhysd/vim-color-spring-night'
Plug 'junegunn/seoul256.vim'
Plug 'dracula/vim'
Plug 'itchyny/lightline.vim'
" }}}

" {{{ interface
Plug 'Shougo/neomru.vim'
Plug 'Shougo/denite.nvim'
Plug 'Jagua/vim-denite-ghq'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

Plug 'itchyny/vim-cursorword'
Plug 'airblade/vim-gitgutter'

Plug 'airblade/vim-rooter'
let g:rooter_use_lcd = 1
" }}}

" {{{ edit
Plug 'scrooloose/syntastic'
Plug 'editorconfig/editorconfig-vim'

Plug 'maralla/completor.vim'

Plug 'osyo-manga/vim-anzu'
nmap n <Plug>(anzu-n-with-echo)
nmap N <Plug>(anzu-N-with-echo)
nmap * <Plug>(anzu-star-with-echo)
nmap # <Plug>(anzu-sharp-with-echo)

Plug 'tyru/caw.vim', { 'on': ['<Plug>(caw:hatpos:toggle)'] }
nmap <Space>c <Plug>(caw:hatpos:toggle)
vmap <Space>c <Plug>(caw:hatpos:toggle)

Plug 'rhysd/devdocs.vim', { 'on': ['DevDocs', 'DevDocsAll', '<Plug>(devdocs-under-cursor)'] }
augroup devdocs
    autocmd!
    autocmd FileType c,cpp,php,ruby nmap <buffer>K <Plug>(devdocs-under-cursor)
augroup END

Plug 'junegunn/vim-easy-align', { 'on': ['EasyAlign'] }

Plug 'mattn/sonictemplate-vim', { 'on': ['Template'] }

Plug 'cohama/lexima.vim'
Plug 'tpope/vim-surround'
" }}}

" {{{ misc
Plug 'Shougo/context_filetype.vim'
Plug 'tpope/vim-fugitive'
Plug 'kana/vim-tabpagecd'
Plug 'vim-scripts/mru.vim'
" }}}

" {{{ language

"" tmux
Plug 'tmux-plugins/vim-tmux'

"" {{{ golang
Plug 'fatih/vim-go', { 'for': 'go' }
let g:go_fmt_command = 'goimports'
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:syntastic_go_checkers = ['govet', 'golint']
"" }}}

"" {{{ cpp c++
Plug 'rhysd/vim-clang-format', { 'for': ['c', 'cpp'] }
augroup clang-format
    autocmd!
    autocmd FileType c,cpp nnoremap <buffer><Leader>cf :<C-u>ClangFormat<CR>
    autocmd FileType c,cpp vnoremap <buffer><Leader>cf :ClangFormat<CR>
    autocmd FileType c,cpp map <buffer><Leader>f <Plug>(operator-clang-format)
augroup END
"" }}}

"" {{{ haskell
Plug 'eagletmt/ghcmod-vim', { 'for': 'haskell' }
nnoremap <Leader>t :<C-u>GhcModType<CR>
nnoremap <Leader>T :<C-u>GhcModTypeClear<CR>

Plug 'eagletmt/neco-ghc', { 'for': 'haskell' }
augroup haskell
    autocmd!
    autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
augroup END
Plug 'dag/vim2hs', { 'for': 'haskell' }
let g:haskell_conceal = 0
"" }}}

"" {{{ rust
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
" let g:rustfmt_autosave = 1

Plug 'racer-rust/vim-racer', { 'for': 'rust' }
"" }}}

"" {{{ python
if executable('flake8')
    let g:syntastic_python_checkers = ['flake8']
    let g:syntastic_python_flake8_args = '--max-line-length=120'
elseif executable('pyflakes') && executable('pep8')
    let g:syntastic_python_checkers = ['pyflakes', 'pep8']
end
if executable('autopep8')
    function! s:preserve_autopep8(cmd)
    endfunction
    function! s:autopep8() abort
        silent %!autopep8 -
    endfunction
    command! Autopep8 call s:autopep8()
end
"" }}}

"" {{{ crystal
Plug 'rhysd/vim-crystal', { 'for': 'crystal' }
""

call plug#end() " }}}

filetype plugin indent on

"" plugin settings

" denite.nvim
if executable('rg')
    " replace file_rec
    call denite#custom#var('file_rec', 'command', ['rg', '--files', '--glob', '!.git', ''])
    " replace grep
    call denite#custom#var('grep', 'command', ['rg'])
    call denite#custom#var('grep', 'default_opts', ['--vimgrep', '--no-heading'])
    call denite#custom#var('grep', 'recursive_opts', [])
    call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
    call denite#custom#var('grep', 'separator', ['--'])
    call denite#custom#var('grep', 'final_opts', [])
endif

nnoremap [denite] <Nop>
nmap <Space>d [denite]
nnoremap <silent> [denite]m :<C-u>Denite file_mru<CR>
nnoremap <silent> [denite]g :<C-u>Denite grep<CR>
nnoremap <silent> [denite]r :<C-u>Denite -resume<CR>
nnoremap <silent> [denite]o :<C-u>Denite outline<CR>

function! s:is_git_repo() abort
    if executable('git')
        call system('git rev-parse --is-inside-work-tree &>/dev/null')
        if v:shell_error == 0
            return 1
        endif
    endif
    return 0
endfunction

function! s:fzf_files() abort
    if s:is_git_repo()
        GFiles
    else
        Files
    endif
endfunction

command! FZFMru call fzf#run({
\ 'source':  reverse(s:all_files()),
\ 'sink':    'edit',
\ 'options': '-m -x +s',
\ 'down':    '40%' })

function! s:all_files()
  return extend(
  \ filter(copy(v:oldfiles),
  \        "v:val !~ 'fugitive:\\|NERD_tree\\|^/tmp/\\|.git/'"),
  \ map(filter(range(1, bufnr('$')), 'buflisted(v:val)'), 'bufname(v:val)'))
endfunction

nnoremap [fzf] <Nop>
nmap <Space>d [fzf]
nnoremap <silent> [fzf]f :<C-u>call <SID>fzf_files()<CR>
nnoremap <silent> [fzf]m :<C-u>FZFMru<CR>
nnoremap <silent> [fzf]b :<C-u>Buffers<CR>
nnoremap <silent> [fzf]l :<C-u>BLines<CR>

nnoremap <silent> grep :<C-u>Denite grep<CR>
nnoremap <silent> ghq :<C-u>Denite ghq<CR>

"" lexima.vim
let g:lexima_no_default_rules = 1
call lexima#set_default_rules()
call lexima#add_rule({'char': "'", 'filetype': ['rust']})

" language support {{{

"" OCaml
if executable('opam')
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
    nnoremap <Leader>t :<C-u>MerlinTypeOf<CR>
    vnoremap <Leader>t :<C-u>MerlinTypeOfSel<CR>
    nnoremap <Leader>n :<C-u>MerlinGrowEnclosing<CR>
    nnoremap <Leader>p :<C-u>MerlinShrinkEnclosing<CR>
    nnoremap <Leader>r <Plug>(MerlinRename)
    nnoremap <Leader>d :<C-u>MerlinDestruct<CR>
    nnoremap <Leader>o :<C-u>MerlinOutline<CR>
    nnoremap <Leader>gd :<C-u>MerlinLocate<CR>
  endfunction

  if executable('ocp-indent')
      command! OcpIndent call s:ocaml_format()
  endif

  augroup ocaml
    autocmd!
    autocmd FileType ocaml call s:ocaml_setup()
  augroup END
endif
" }}}

" 基本設定 {{{
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 全般
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:mapleader = ','
let &backupdir = expand('~/.vim/_backup')
set clipboard=unnamed,unnamedplus
set t_vb=
set visualbell
set noerrorbells
set wildmenu wildmode=list:longest

"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" display
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" set termguicolors
" colorscheme spring-night
syntax enable
set background=dark
colorscheme iceberg

set cmdheight=2
set hidden
set nonumber
set scrolloff=5
set wrap
set display=lastline
set showbreak=+\
set breakindent
" set cursorline
" set cursorcolumn
set colorcolumn=80
set ruler
set tabstop=4
set softtabstop=4
set expandtab
set smarttab
set laststatus=2
set foldmethod=marker
set foldlevel=100
set list
set listchars=tab:▸\ ,trail:-,eol:¬
" 補完ポップアップの高さ設定
set pumheight=10
syntax on

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

nnoremap <C-]> g<C-]>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" input
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
set backspace=indent,eol,start
set autoindent
set shiftwidth=4
set showmatch
set matchtime=1
set whichwrap=b,s,h,l,<,>,[,]
set smarttab

nnoremap ; :
vnoremap ; :
nnoremap : ;
vnoremap : ;
nnoremap q; q:
vnoremap q; q:

" }}}

