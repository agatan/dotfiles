if !1 | finish | endif
if has('vim_starting')
  set encoding=utf-8
endif
scriptencoding utf8

" F1で.vimrcの編集に
nnoremap <Space>. :edit $MYVIMRC<CR>
" :ReloadVimrcで設定を反映
command! ReloadVimrc source $MYVIMRC

" reset autogroup
augroup vimrc
    autocmd!
augroup END

if has('vim_starting')
	set runtimepath+=~/.nvim/bundle/neobundle.vim
endif

let $VIM_NEOBUNDLE_PLUGIN_DIR = '~/.nvim/bundle'

let s:neobundle_plugins_dir =
            \ expand(exists('$VIM_NEOBUNDLE_PLUGIN_DIR') ? $VIM_NEOBUNDLE_PLUGIN_DIR 
            \                                            : '~/.nvim/bundle')

if !executable('git')
    echo 'Please install git.'
    finish
endif


if !isdirectory(s:neobundle_plugins_dir . '/neobundle.vim')
    echo 'Please install neobundle.vim'
    function! s:install_neobundle()
        if input('Install neobundle.vim? [Y/N] : ') ==# 'Y'
            if !isdirectory(s:neobundle_plugins_dir)
                call mkdir(s:neobundle_plugins_dir, 'p')
            endif

            execute '!git clone git://github.com/Shougo/neobundle.vim '
                        \ . s:neobundle_plugins_dir . '/neobundle.vim'
            echo 'neobundle installed. Please restart vim'
        else
            echo 'Canceled.'
        endif
    endfunction
    augroup install-neobundle
        autocmd!
        autocmd VimEnter * call s:install_neobundle()
    augroup END
    finish
endif

call neobundle#begin(expand('~/.nvim/bundle'))

NeoBundleFetch 'Shougo/neobundle.vim'


"""" Plugins

NeoBundle 'Shougo/vimproc.vim', {
			\ 'build' : {
			\	'windows' : 'tools\\update-dll-mingw',
			\	'cygwin' : 'make -f make_cygwin.mak',
			\	'mac' : 'make -f make_mac.mak',
			\	'linux' : 'make',
			\	'unix' : 'gmake',
			\    },
			\ }

"" 必須系
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/unite-outline'
NeoBundle 'tsukkee/unite-tag'
NeoBundle 'Shougo/neomru.vim'
NeoBundle 'vim-jp/vital.vim'
NeoBundle 'wakatime/vim-wakatime'

NeoBundle 'Yggdroot/indentLine'
NeoBundle 'tyru/caw.vim'
NeoBundle 'junegunn/vim-easy-align'
NeoBundle 'Shougo/neosnippet'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'thinca/vim-quickrun'
NeoBundle 'thinca/vim-ref'
NeoBundle 'itchyny/lightline.vim'
NeoBundle 'tpope/vim-surround'
NeoBundle 'osyo-manga/shabadou.vim'
NeoBundle 'osyo-manga/vim-watchdogs'
NeoBundle 'cohama/vim-hier'
NeoBundle 'vim-jp/vimdoc-ja'

NeoBundle 'airblade/vim-rooter'
let g:rooter_use_lcd = 1

NeoBundle 'Lokaltog/vim-easymotion'
nmap mm <Plug>(easymotion-s2)

" NeoBundle 'Shougo/deoplete.nvim'

" git
NeoBundle 'airblade/vim-gitgutter'
NeoBundleLazy 'lambdalisue/vim-gita', {
            \ 'autoload': {
            \   'commands': ['Gita'],
            \ }}



"" 各言語

" c, c++
NeoBundleLazy 'vim-jp/cpp-vim', {
            \ 'autoload' : {'filetypes': 'cpp'}
            \ }
NeoBundleLazy 'osyo-manga/vim-stargate', {
            \ 'autoload' : {'filetypes' : ['c', 'cpp'] }
            \ }
NeoBundleLazy 'justmao945/vim-clang', {
      \ 'autoload' : {'filetypes' : ['cpp']}
      \ }
NeoBundleLazy 'agatan/vim-sort-include', {
      \ 'autoload' : {'filetypes' : ['c', 'cpp']}
      \ }

" golang
NeoBundleLazy 'fatih/vim-go', {
            \ 'autoload': {"filetypes": ["go"]}}

" rust
if executable('cargo') && isdirectory(expand('~/rust/rust/src'))
    NeoBundle 'rust-lang/rust.vim'
    NeoBundle 'phildawes/racer', {
                \ 'build': {
                \   'mac': 'cargo build --release',
                \   'unix': 'cargo build --release',
                \ }
                \}
    set hidden
    let g:racer_cmd = expand('~/.vim/bundle/racer/target/release/racer')
    let $RUST_SRC_PATH=expand('~/rust/rust/src/')

    NeoBundleLazy 'rhysd/rust-doc.vim', {
                \ 'autoload': {'filetypes': 'rust' }
                \ }
    let g:rust_doc#downloaded_rust_doc_dir = '~/rust/rust-1.0.0-i686-unknown-linux-gnu/rust-docs'
endif


" sphinx
NeoBundleLazy 'Rykka/riv.vim',
            \ {'autoload': {"filetypes": ["rst"]}}


" haskell
" NeoBundleLazy 'dag/vim2hs',
"             \ {'autoload': {"filetypes": ['haskell']}}
NeoBundleLazy 'eagletmt/ghcmod-vim',
            \ {'autoload': {'filetypes': ['haskell']}}
NeoBundleLazy 'ujihisa/unite-haskellimport',
            \ {'autoload': {'filetypes': ['haskell']}}
NeoBundleLazy 'itchyny/vim-haskell-indent',
            \ {'autoload': {'filetypes': ['haskell']}}

" markdown
NeoBundleLazy 'kannokanno/previm',
            \ {'autoload' : {"filetypes": ["markdown"]}}
NeoBundleLazy 'plasticboy/vim-markdown',
            \ {'autoload' : {"filetypes": ["markdown"]}}
NeoBundleLazy 'tyru/open-browser.vim',
            \ {'autoload' : {"filetypes": ["markdown"]}}

" VimL
NeoBundle 'rbtnn/vimconsole.vim'
NeoBundle 'thinca/vim-prettyprint'

" OCaml
" NeoBundleLazy 'cohama/the-ocamlspot.vim',
"             \ {'autoload': {'filetypes': 'ocaml' }}
" merlin
let g:opamshare = substitute(system('opam config var share'), '\n$', '', '''')
execute "set rtp+=" . g:opamshare . "/merlin/vim"
augroup merlin
  autocmd!
  autocmd FileType ocaml nnoremap <Space>t :<C-u>MerlinTypeOf<CR>
augroup END

" nim
NeoBundleLazy 'zah/nim.vim',
            \ {'autoload': {'filetypes': ['nim'] }}

" Crystal
NeoBundle 'rhysd/vim-crystal'

" ATS2
NeoBundleLazy 'vim-scripts/ats-lang-vim',
      \ {'autoload': {'filetypes': ['ats']}}

" Idris
NeoBundleLazy 'idris-hackers/idris-vim',
      \ {'autoload': {'filetypes': ['idris']}}


"" color
NeoBundle 'w0ng/vim-hybrid'
"NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'croaker/mustang-vim'
NeoBundle 'jeffreyiacono/vim-colors-wombat'
NeoBundle 'nanotech/jellybeans.vim'
NeoBundle 'vim-scripts/Lucius'
NeoBundle 'vim-scripts/Zenburn'
NeoBundle 'mrkn/mrkn256.vim'
NeoBundle 'jpo/vim-railscasts-theme'
NeoBundle 'therubymug/vim-pyte'
NeoBundle 'tomasr/molokai'
NeoBundle 'cocopon/iceberg.vim'

NeoBundle 'ujihisa/unite-colorscheme'


call neobundle#end()


NeoBundleCheck

filetype plugin indent on


""" 各プラグインの設定

" deoplete
" let g:deoplete#enable_at_startup = 1

" caw.vim
" コメントアウトを切り替えるマッピング
nmap <Space>c <Plug>(caw:I:toggle)
vmap <Space>c <Plug>(caw:I:toggle)

" <leader>C でコメントアウトを解除
nmap <Space>C <Plug>(caw:I:uncomment)
vmap <Space>C <Plug>(caw:I:uncomment)

" unite

let g:unite_enable_split_vertically = 1
nnoremap ,uf :<C-u>Unite<space>file<CR>
nnoremap ,ub :<C-u>Unite<space>buffer<CR>
nnoremap ,um :<C-u>Unite<space>file_mru<CR>



" For snippet_complete marker.
if has('conceal')
  set conceallevel=2 concealcursor=i
endif


" quickrun

let g:quickrun_config = {
\   '_' : {
\       'outputter/buffer/split' : ':botright 8sp',
\       'outputter/buffer/close_on_empty': 1,
\       'runner' : 'vimproc',
\       'runner/vimproc/updatetime' : 60,
\       'outputter' : 'error',
\       'outputter/error/success' : 'buffer',
\   },
\   'ruby' : {
\       'command': '/Users/nao/.rbenv/shims/ruby'
\   },
\   'cpp' : {
\       'command': 'clang++',
\       'cmdopt': '--std=c++14 '
\   },
\   'c' : {
\       'command': 'gcc'
\   },
\   'cpp/wandbox' : {
\	'runner' : 'wandbox',
\	'runner/wandbox/compiler' : 'clang-head',
\	'runner/wandbox/options' : 'warning,c++1y,boost-1.55',
\   },
\   'cpp/watchdogs_checker' : {
\   	'type' : 'watchdogs_checker/clang++03',
\   },
\   'watchdogs_checker/clang++03' : {
\      'cmdopt' : '-Wall --std=c++14',
\   },
\   'watchdogs_checker/g++' : {
\       'cmdopt' : '-Wall -std=c++14',
\   },
\   'watchdogs_checker/cargo' : {
\       'command' : 'cargo',
\       'cmdopt' : 'build',
\       'exec' : '%c %o',
\   },
\   'watchdogs_checker/cargo_test' : {
\       'command' : 'cargo',
\       'cmfopt' : 'test',
\       'exec' : '%c %o',
\   },
\   'rust/watchdogs_checker' : {
\       'type' : 'watchdogs_checker/cargo',
\   },
\   'watchdogs_checker/golint' : {
\       'command': 'golint',
\       'exec': '%c %o %s:p',
\       'errorformat': '%f:%l:%c: %m,%-G%.%#',
\   },
\   'go/watchdogs_checker' : {
\       'type': 'watchdogs_checker/golint'
\   },
\}




" neosnippet

" Plugin key-mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

"" indentLine
let g:indentLine_color_term = 238
let g:indentLine_color_gui = '#708090'
let g:indentLine_char = '¦'

" golang

filetype off
filetype plugin indent off
if $GOROOT !=? ''
    set rtp+=$GOROOT/misc/vim/
endif
filetype plugin indent on
syntax on
autocmd vimrc FileType go autocmd BufWritePre <buffer> GoImports
exe 'set rtp+='.globpath($GOPATH, 'src/github.com/nsf/gocode/vim')
set completeopt=menu,preview

" C++

augroup cpp-path
    autocmd!
    autocmd FileType cpp execute 'setlocal path=.,/usr/include,/usr/local/include' .
                \ join(filter(split(glob('/usr/include/**/c++/*'), '\n'),
                \             'isdirectory(v:val)'),
                \      ',')
augroup END
set cinoptions+=:0,g0

let g:clang_compilation_database = './build'
" let g:clang_c_completeopt = 'menuone'
" let g:clang_cpp_complete_opt = 'menuone'
let g:clang_cpp_complete_opt = 'menuone,preview,noinsert'
let g:clang_auto = 0
let g:clang_cpp_options = '-std=c++1z'
let g:clang_format_auto = 1
let g:clang_format_style = 'LLVM'
let g:clang_check_syntax_auto = 1

augroup sort-include
  autocmd!
  autocmd BufWritePre *.{c,cpp,h,hpp} SortInclude
augroup END

" nim
augroup nim
    autocmd!
    autocmd BufNewFile,BufRead *.nim set filetype=nim
    autocmd BufNewFile,BufRead *.nim setlocal shiftwidth=2 tabstop=2 softtabstop=2
    autocmd FileType nim setlocal shiftwidth=2 tabstop=2 softtabstop=2
augroup END

" markdown
augroup PrevimSettings
    autocmd!
    autocmd BufNewFile,BufRead *.{md,mdwn,mkdn,mark*} set filetype=markdown
augroup END

" vim script
let g:quickrun_config['vim/watchdogs_checker'] = {
\       'type' : executable('vint') ? 'watchdogs_checker/vint' : '',
\   }
let g:quickrun_config['watchdogs_checker/vint'] = {
\       'command' : 'vint',
\       'exec' : '%c %o %s:p',
\   }


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


" 開いているファイルと同じディレクトリに常に移動する
" projeect root にいたいのでコメントアウト
" autocmd vimrc BufEnter * execute 'lcd ' . fnameescape(expand('%:p:h'))


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
augroup vimrc
    autocmd FileType ruby set ts=2 sw=2 softtabstop=2
    autocmd FileType eruby set ts=2 sw=2 softtabstop=2
augroup END


" 英字配列だとコロンがつらい
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;


"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" メモや一時ファイル関連
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
command! Todo edit ~/Dropbox/memo/todo.md
command! Memo edit ~/Dropbox/memo/memo.md


"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" VimL
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
command! -bang -nargs=* PluginTest call PluginTest()
function! PluginTest()
    execute '!vim -u NONE -i NONE -N --cmd "set runtimepath+=' . getcwd() . '"'
endfunction



"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Haskell
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup vimrc
    autocmd FileType haskell set sw=2 ts=2 softtabstop=2
    autocmd FileType haskell nnoremap <Space>t :<C-u>GhcModType<CR>
augroup END
