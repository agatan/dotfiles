set encoding=utf-8
scriptencoding utf8

nnoremap <Space>. :edit $MYVIMRC<CR>

" :ReloadVimrc load $MYVIMRC
command! ReloadVimrc source $MYVIMRC

" neovim python3 client
if isdirectory(expand($HOME . '/venvs/neovim/'))
  let g:python3_host_prog = expand($HOME . '/venvs/neovim/bin/python3')
else
  throw 'python3 venv for neovim is not found'
endif

augroup myvimrc
  autocmd!
augroup END

"" Plugins

filetype off

let s:vim_plug_path = expand($HOME . '/.vim/plugged/')
if has('vim_starting')
  execute 'set runtimepath+=' . s:vim_plug_path . 'vim-plug'
  if !isdirectory(s:vim_plug_path . 'vim-plug')
    echo 'install vim-plug...'
    call mkdir(s:vim_plug_path, 'p')
    call system('git clone https://github.com/junegunn/vim-plug ' . s:vim_plug_path . 'vim-plug/autoload')
  end
endif
call plug#begin(s:vim_plug_path) " {{{

Plug 'vim-jp/vimdoc-ja'

" {{{ color
Plug 'rhysd/try-colorscheme.vim', { 'on': 'TryColorscheme' }
Plug 'cocopon/iceberg.vim'
Plug 'rhysd/wallaby.vim'
Plug 'w0ng/vim-hybrid'
Plug 'nanotech/jellybeans.vim'
Plug 'itchyny/lightline.vim'
" }}}

" {{{ interface
Plug 'Shougo/neomru.vim'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

Plug 'itchyny/vim-cursorword'
" Plug 'airblade/vim-gitgutter'

Plug 'airblade/vim-rooter'
let g:rooter_use_lcd = 1

if executable('rg')
    Plug 'mileszs/ack.vim'
    let g:ackprg = 'rg --vimgrep'
endif
" }}}

" {{{ edit
Plug 'editorconfig/editorconfig-vim'
Plug 'thinca/vim-qfreplace', { 'on': ['Qfreplace'] }

set completeopt=noinsert,noselect,menuone
Plug 'Shougo/echodoc.vim'

Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'

Plug 'osyo-manga/vim-anzu'
nmap n <Plug>(anzu-n-with-echo)
nmap N <Plug>(anzu-N-with-echo)
nmap * <Plug>(anzu-star-with-echo)
nmap # <Plug>(anzu-sharp-with-echo)
augroup myvimrc
  autocmd CmdlineEnter [/\?] :set hlsearch
  autocmd CmdlineLeave [/\?] :set nohlsearch
augroup END

Plug 'tyru/caw.vim', { 'on': ['<Plug>(caw:hatpos:toggle)'] }
nmap <Space>c <Plug>(caw:hatpos:toggle)
vmap <Space>c <Plug>(caw:hatpos:toggle)

Plug 'junegunn/vim-easy-align', { 'on': ['EasyAlign'] }

Plug 'mattn/sonictemplate-vim', { 'on': ['Template'] }

" Plug 'cohama/lexima.vim'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-surround'
" }}}

" {{{ misc
Plug 'Shougo/context_filetype.vim'
Plug 'tpope/vim-fugitive'
Plug 'kana/vim-tabpagecd'
Plug 'vim-scripts/mru.vim'

Plug 'thinca/vim-quickrun'
" }}}

" {{{ language

"" tmux
Plug 'tmux-plugins/vim-tmux'

"" fish
Plug 'dag/vim-fish'

"" {{{ golang
Plug 'fatih/vim-go', { 'for': ['go'] }
let g:go_fmt_command = 'goimports'
let g:go_def_mapping_enabled = 0
let g:go_doc_keywordprg_enabled = 0
"" }}}

"" {{{ cpp c++
Plug 'rhysd/vim-clang-format', { 'for': ['c', 'cpp'] }
augroup myvimrc
  autocmd FileType c,cpp nnoremap <buffer><Leader>cf :<C-u>ClangFormat<CR>
  autocmd FileType c,cpp vnoremap <buffer><Leader>cf :ClangFormat<CR>
  autocmd FileType c,cpp map <buffer><Leader>f <Plug>(operator-clang-format)
augroup END
Plug 'justmao945/vim-clang', { 'for': ['c', 'cpp'] }
let g:clang_auto = 0
let g:clang_cpp_options = '-std=c++14'
let g:clang_diagsopt = ''
"" }}}

"" {{{ haskell
Plug 'eagletmt/ghcmod-vim', { 'for': 'haskell' }
nnoremap <Leader>t :<C-u>GhcModType<CR>
nnoremap <Leader>T :<C-u>GhcModTypeClear<CR>

Plug 'eagletmt/neco-ghc', { 'for': 'haskell' }
augroup myvimrc
  autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
augroup END
Plug 'dag/vim2hs', { 'for': 'haskell' }
let g:haskell_conceal = 0
"" }}}

"" {{{ rust
" Plug 'rust-lang/rust.vim', { 'for': 'rust' }
" let g:rustfmt_autosave = 1

"" }}}

"" {{{ python
Plug 'heavenshell/vim-pydocstring'
"" }}}

"" {{{ crystal
Plug 'rhysd/vim-crystal', { 'for': 'crystal' }
"" }}}

"" {{{ javascript
Plug 'flowtype/vim-flow'
Plug 'pangloss/vim-javascript', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'othree/yajs.vim', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'othree/es.next.syntax.vim', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'maxmellon/vim-jsx-pretty', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'posva/vim-vue', { 'for': ['javascript', 'vue'] }
"" }}}

"" {{{ typescript
Plug 'leafgarland/typescript-vim', { 'for': ['typescript', 'typescript.tsx'] }
Plug 'prettier/vim-prettier', { 'do': 'npm install' }
let g:prettier#autoformat = 0
"" }}}

"" {{{ web / html / css
Plug 'othree/html5.vim'
Plug 'mattn/emmet-vim'
"" }}}

"" {{{ yaml
Plug 'mrk21/yaml-vim'
"" }}}

"" {{{ toml
Plug 'cespare/vim-toml'
"" }}}

call plug#end() " }}}

filetype plugin indent on

"" plugin settings

function! s:is_git_repo() abort
  if executable('git')
    call system('git rev-parse --is-inside-work-tree &>/dev/null')
    if v:shell_error == 0
      return 1
    endif
  endif
  return 0
endfunction

let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
 \ 'bg':      ['bg', 'TabLine'],
 \ 'hl':      ['fg', 'Statement'],
 \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
 \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
 \ 'hl+':     ['fg', 'Statement'],
 \ 'info':    ['fg', 'PreProc'],
 \ 'border':  ['fg', 'Ignore'],
 \ 'prompt':  ['fg', 'Conditional'],
 \ 'pointer': ['fg', 'Exception'],
 \ 'marker':  ['fg', 'Keyword'],
 \ 'spinner': ['fg', 'Label'],
 \ 'header':  ['fg', 'Comment'] }

command! -bang -nargs=* Rg
 \ call fzf#vim#grep(
 \   'rg --column --line-number --no-heading --color always --colors path:fg:0xb2,0x94,0xbb --colors line:fg:0x6c,0x7a,0x80 --colors column:fg:0x6c,0x7a,0x80 --smart-case --hidden --glob "!/.git" '.shellescape(<q-args>), 1,
 \   <bang>0 ? fzf#vim#with_preview({'options': '--color dark,hl:#8abeb7,hl+:#8abeb7,prompt:#8abeb7,pointer:#8abeb7 --delimiter : --nth 4..'}, 'up:60%')
 \           : fzf#vim#with_preview({'options': '--color dark,hl:#8abeb7,hl+:#8abeb7,prompt:#8abeb7,pointer:#8abeb7 --delimiter : --nth 4..'}, 'right:50%:hidden', '?'),
 \   <bang>0)


function! s:fzf_files() abort
  if s:is_git_repo()
    GFiles
  else
    Files
  endif
endfunction

function! s:all_files()
  return extend(
       \ filter(copy(v:oldfiles),
       \        "v:val !~# 'fugitive:\\|NERD_tree\\|^/tmp/\\|.git/'"),
       \ map(filter(range(1, bufnr('$')), 'buflisted(v:val)'), 'bufname(v:val)'))
endfunction

nnoremap [fzf] <Nop>
nmap <Space>f [fzf]
" nnoremap <silent> [fzf]f :<C-u>call <SID>fzf_files()<CR>
nnoremap <silent> [fzf]g :<C-u>GitFiles<CR>
nnoremap <silent> [fzf]f :<C-u>Files<CR>
nnoremap <silent> [fzf]m :<C-u>History<CR>
nnoremap <silent> [fzf]b :<C-u>Buffers<CR>
nnoremap <silent> [fzf]l :<C-u>BLines<CR>
nnoremap <silent> [fzf]t :<C-u>Tags<CR>


"" {{{ lightline.vim
function! CocCurrentFunction()
    return get(b:, 'coc_current_function', '')
endfunction

let g:lightline = {
      \ 'colorscheme': 'powerline',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'cocstatus', 'currentfunction', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'cocstatus': 'coc#status',
      \   'currentfunction': 'CocCurrentFunction'
      \ },
      \ }
"" }}}

" echodoc {{{
let g:echodoc#enable_at_startup = 1
" }}}

" language support {{{

function! s:on_lsp_buffer_enabled() abort
  setlocal omnifunc=lsp#complete
  setlocal signcolumn=yes
  nmap <buffer> gd <plug>(lsp-definition)
  nmap <buffer> <f2> <plug>(lsp-rename)
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
    let l:now_line = line('.')
    execute ':%! ocp-indent'
    execute ':' . l:now_line
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

  augroup myvirmc
    autocmd FileType ocaml call s:ocaml_setup()
  augroup END
endif


"" ruby
augroup myvimrc
  autocmd FileType ruby setlocal regexpengine=1
augroup END

"" Crystal
augroup myvimrc
  autocmd FileType crystal setlocal regexpengine=1
augroup END

" }}}

" 基本設定 {{{
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 全般
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:mapleader = ','
" let &backupdir = expand('~/.vim/_backup')
set clipboard=unnamed,unnamedplus
set t_vb=
set visualbell
set noerrorbells
set wildmenu wildmode=list:longest
set updatetime=300
set shortmess+=c

"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" display
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" set termguicolors
set t_ut=
syntax enable
set background=dark
silent! colorscheme iceberg
set noshowmode

set signcolumn=yes
set cmdheight=2
set hidden
set nonumber
set scrolloff=5
set wrap
set display=lastline
" set showbreak=+\
set breakindent
" set cursorline
" set cursorcolumn
" set colorcolumn=80
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
augroup grepquickfix
  autocmd!
  autocmd QuickFixCmdPost *grep* cwindow
augroup END

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

