set encoding=utf-8
scriptencoding utf8

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
    if !isdirectory(s:vim_plug_path . 'vim-plug')
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
Plug 'itchyny/lightline.vim'
" }}}

" {{{ interface
Plug 'Shougo/neomru.vim'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

Plug 'itchyny/vim-cursorword'
Plug 'airblade/vim-gitgutter'

Plug 'airblade/vim-rooter'
let g:rooter_use_lcd = 1
" }}}

" {{{ edit
Plug 'neomake/neomake'
Plug 'editorconfig/editorconfig-vim'
Plug 'thinca/vim-qfreplace', { 'on': ['Qfreplace'] }

Plug 'maralla/completor.vim'
" Plug 'maralla/completor-neosnippet'
" Plug 'Shougo/neosnippet'
" Plug 'Shougo/neosnippet-snippets'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'Shougo/echodoc.vim'

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

Plug 'rhysd/devdocs.vim', { 'on': ['DevDocs', 'DevDocsAll', '<Plug>(devdocs-under-cursor)'] }
augroup myvimrc
    autocmd FileType c,cpp,php,ruby nmap <buffer>K <Plug>(devdocs-under-cursor)
augroup END

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

"" {{{ golang
Plug 'fatih/vim-go', { 'for': 'go' }
let g:go_fmt_command = 'goimports'
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
Plug 'yosssi/vim-ace', { 'for': 'ace' }
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

"" {{{ dlang
Plug 'idanarye/vim-dutyl', { 'for': 'd' }
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
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
let g:rustfmt_autosave = 1

Plug 'racer-rust/vim-racer', { 'for': 'rust' }
"" }}}

"" {{{ python
if executable('flake8')
    let g:syntastic_python_flake8_args = '--max-line-length=120'
elseif executable('pyflakes') && executable('pep8')
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

""" {{{ ruby
Plug 'vim-ruby/vim-ruby'
""" }}}

"" {{{ crystal
Plug 'rhysd/vim-crystal', { 'for': 'crystal' }
"" }}}

"" {{{ javascript
Plug 'flowtype/vim-flow'
Plug 'pangloss/vim-javascript', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'othree/yajs.vim', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'othree/es.next.syntax.vim', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'maxmellon/vim-jsx-pretty', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'ternjs/tern_for_vim', { 'for': ['javascript', 'javascript.jsx'], 'do': 'npm install' }
Plug 'posva/vim-vue', { 'for': ['javascript', 'vue'] }
"" }}}

"" {{{ web / html / css
Plug 'othree/html5.vim'
"" }}}

"" {{{ yaml
Plug 'mrk21/yaml-vim'
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
nnoremap <silent> [fzf]f :<C-u>Files<CR>
nnoremap <silent> [fzf]m :<C-u>History<CR>
nnoremap <silent> [fzf]b :<C-u>Buffers<CR>
nnoremap <silent> [fzf]l :<C-u>BLines<CR>
nnoremap <silent> [fzf]t :<C-u>Tags<CR>

" fugitive
nnoremap <Space>g :<C-u>Ggrep

" Neomake
augroup myvimrc
    autocmd BufWritePost * Neomake
    autocmd ColorScheme *
                \ highlight NeomakeError ctermfg=lightred |
                \ highlight NeomakeErrorSign ctermfg=red |
                \ highlight NeomakeWarningSign ctermfg=yellow
augroup END


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

  augroup myvimrc
    autocmd FileType ocaml call s:ocaml_setup()
  augroup END
endif

"" C++
let g:neomake_cpp_enabled_markers = ['clang']
let g:neomake_c_enabled_markers = ['clang']


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
set t_ut=
syntax enable
set background=dark
silent! colorscheme iceberg " suppress error message even if hybrid is not installed.

set cmdheight=2
set hidden
set nonumber
set scrolloff=5
set wrap
set display=lastline
set showbreak=+\
set breakindent
" set cursorline
set cursorcolumn
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
" set completeopt-=preview
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
let g:UltiSnipsExpandTrigger = '<c-k>'
" inoremap <expr> <Tab> pumvisible() ? '<C-y>' :  '<CR>'

nnoremap ; :
vnoremap ; :
nnoremap : ;
vnoremap : ;
nnoremap q; q:
vnoremap q; q:

" }}}

