if !1 | finish | endif
scriptencoding utf-8

" F1で.vimrcの編集に
nnoremap <F1> :edit ~/.vimrc<CR>
" :ReloadVimrcで設定を反映
command! ReloadVimrc source ~/.vimrc

if has('vim_starting')
	set runtimepath+=~/.vim/bundle/neobundle.vim
endif

let $VIM_NEOBUNDLE_PLUGIN_DIR = '~/.vim/bundle'

let s:neobundle_plugins_dir =
            \ expand(exists("$VIM_NEOBUNDLE_PLUGIN_DIR") ? $VIM_NEOBUNDLE_PLUGIN_DIR 
            \                                            : '~/.vim/bundle')

if !executable('git')
    echo "Please install git."
    finish
endif


if !isdirectory(s:neobundle_plugins_dir . "/neobundle.vim")
    echo "Please install neobundle.vim"
    function! s:install_neobundle()
        if input("Install neobundle.vim? [Y/N] : ") == "Y"
            if !isdirectory(s:neobundle_plugins_dir)
                call mkdir(s:neobundle_plugins_dir, "p")
            endif

            execute "!git clone git://github.com/Shougo/neobundle.vim"
                        \ . s:neobundle_plugins_dir . "/neobundle.vim"
            echo "neobundle installed. Please restart vim"
        else
            echo "Canceled."
        endif
    endfunction
    augroup install-neobundle
        autocmd!
        autocmd VimEnter * call s:install_neobundle()
    augroup END
    finish
endif

call neobundle#begin(expand('~/.vim/bundle'))

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
NeoBundle 'tyru/caw.vim'
NeoBundle 'Shougo/neosnippet'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'Shougo/vimshell.vim'
NeoBundle 'scrooloose/syntastic.git'
NeoBundle 'thinca/vim-quickrun'
NeoBundle 'thinca/vim-ref'
NeoBundle 'itchyny/lightline.vim'
NeoBundle 'mattn/emmet-vim'
NeoBundle 'tpope/vim-surround'
NeoBundle 'osyo-manga/shabadou.vim'
NeoBundle 'osyo-manga/vim-watchdogs'

" +luaならばneocomplete
if has('lua')
    NeoBundle "Shougo/neocomplete.vim"
else
    NeoBundle "Shougo/neocomplcache"
endif

"" 各言語

" c, c++
NeoBundleLazy 'rhysd/vim-clang-format', {
            \ 'autoload' : {'filetypes': ['c', 'cpp'] }
            \ }

NeoBundleLazy 'vim-jp/cpp-vim', {
            \ 'autoload' : {'filetypes': 'cpp'}
            \ }
NeoBundleLazy 'osyo-manga/vim-stargate', {
            \ 'autoload' : {'filetypes' : ['c', 'cpp'] }
            \ }
NeoBundleLazy 'osyo-manga/vim-marching', {
            \ 'depends' : ['Shougo/vimproc.vim', 'osyo-manga/vim-reunions'],
            \ 'autoload' : {'filetypes' : ['c', 'cpp']}
            \ }

" golang
NeoBundleLazy 'fatih/vim-go', {
            \ "autoload": {"filetypes": ["go"]}}

" rust
NeoBundle 'wting/rust.vim'

" sphinx
NeoBundleLazy 'Rykka/riv.vim',
            \ {"autoload": {"filetypes": ["rst"]}}

" python

NeoBundleLazy 'davidhalter/jedi-vim',
            \ {"autoload": {"filetypes": ["python"]}}
NeoBundleLazy 'nvie/vim-flake8',
            \ {"autoload": {"filetypes": ["python"]}}
NeoBundleLazy 'Yggdroot/indentLine',
            \ {"autoload": {"filetypes": ["python"]}}
NeoBundleLazy 'jmcantrell/vim-virtualenv',
            \ {"autoload": {"filetypes": ["python"]}}


"" color
NeoBundle 'w0ng/vim-hybrid'



call neobundle#end()


NeoBundleCheck

filetype plugin indent on

""" 各プラグインの設定

" caw.vim
let s:hooks = neobundle#get_hooks("caw.vim")
function! s:hooks.on_source(budle)
    " コメントアウトを切り替えるマッピング
    nmap <Leader>c <Plug>(caw:I:toggle)
    vmap <Leader>c <Plug>(caw:I:toggle)

    " <leader>C でコメントアウトを解除
    nmap <Leader>C <Plug>(caw:I:uncomment)
    vmap <Leader>C <Plug>(caw:I:uncomment)
endfunction
unlet s:hooks

" unite

filetype plugin indent on
let g:unite_enable_split_vertically = 1
nnoremap ,uf :<C-u>Unite<space>file<CR>
nnoremap ,ub :<C-u>Unite<space>buffer<CR>
"nnoremap ,um :<C-u>Unite<space>file_mru<CR>

" neocomplete
let s:hooks = neobundle#get_hooks("neocomplete.vim")
function! s:hooks.on_source(bundle)
    let g:neocomplete#enable_at_startup = 1
    let g:neocomplete#enable_ignore_case = 1
    let g:neocomplete#enable_smart_case = 1
    let g:neocomplete#skip_auto_completion_time = ""
    " Set minimum syntax keyword length.
    let g:neocomplete#sources#syntax#min_keyword_length = 3
    let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

    " Define dictionary.
    let g:neocomplete#sources#dictionary#dictionaries = {
                \ 'default' : '',
                \ 'vimshell' : $HOME.'/.vimshell_hist',
                \ }
    if !exists('g:neocomplete#keyword_patterns')
        let g:neocomplete#keyword_patterns = {}
    endif
    let g:neocomplete#keyword_patterns['default'] = '\h\w*'
    " <CR>: close popup and save indent.
    inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
    function! s:my_cr_function()
        return neocomplete#close_popup() . "\<CR>"
        " For no inserting <CR> key.
        "return pumvisible() ? neocomplete#close_popup() : "\<CR>"
    endfunction
    " <TAB>: completion.
    inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
    " <C-h>, <BS>: close popup and delete backword char.
    inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
    inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
endfunction
unlet s:hooks

let s:hooks = neobundle#get_hooks("neocomplcache")
function! s:hooks.on_source(bundle)
    let g:neocomplcache_enable_at_startup = 1
endfunction
unlet s:hooks




" marching.vim
let s:hooks = neobundle#get_hooks("vim-marching")
function! s:hooks.on_post_source(bundle)
    if !empty(g:marching_clang_command) && executable(g:marching_clang_command)
        " 非同期ではなく同期処理で補完
        let g:marching_backend = "sync_clang_command"
    else
        " clangコマンドが実行できない場合は wandbox を使用
        let g:marching_backend = "wandbox"
        let g:marching_clang_command = ""
    endif

    let g:marching#clang_command#options = {
                \ 'cpp': '-std=c++1y'
                \ }

    if !neobundle#is_sourced("neocomplete.vim")
        return
    endif

    " neocomplete.vim と併用する場合の設定
    let g:marching_enable_neocomplete = 1

    if !exists('g:neocomplete#force_omni_input_patterns')
        let g:neocomplete#force_omni_input_patterns = {}
    endif

    let g:neocomplete#force_omni_input_patterns.cpp =
                \ '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'
    let g:neocomplete#force_omni_input_patterns.c =
                \ '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'
endfunction
unlet s:hooks



" SuperTab like snippets behavior.
imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: pumvisible() ? "\<C-n>" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: "\<TAB>"

" For snippet_complete marker.
if has('conceal')
  set conceallevel=2 concealcursor=i
endif


" quickrun

let g:quickrun_config = {
\   "_" : {
\       "outputter/buffer/split" : ":botright 8sp",
\       "outputter/buffer/close_on_empty": 1,
\       "runner" : "vimproc",
\       "runner/vimproc/updatetime" : 60,
\       "outputter" : "error",
\       "outputter/error/success" : "buffer",
\   },
\   "ruby" : {
\       "command": "/Users/nao/.rbenv/shims/ruby"
\   },
\   "cpp" : {
\       "command": "clang++",
\       "cmdopt": "--std=c++1y --stdlib=libc++"
\   },
\   "c" : {
\       "command": "gcc"
\   },
\   "cpp/wandbox" : {
\	"runner" : "wandbox",
\	"runner/wandbox/compiler" : "clang-head",
\	"runner/wandbox/options" : "warning,c++1y,boost-1.55",
\   },
\		"cpp/watchdogs_checker" : {
\			"type" : "watchdogs_checker/clang++",
\		},
\	
\	
\		"watchdogs_checker/clang++" : {
\			"cmdopt" : "-Wall",
\		},
\}


" vim-watchdogs
let s:hooks = neobundle#get_hooks("vim-watchdogs")
function! s:hooks.on_source(bundle)
    let g:watchdogs_check_BufWritePost_enable = 1
endfunction
unlet s:hooks


" neosnippet

" Plugin key-mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)


" python系

autocmd FileType python setlocal omnifunc=jedi#completions
autocmd FileType python setlocal completeopt-=preview

let g:jedi#completions_enabled = 0
let g:jedi#auto_vim_configuration = 0

if !exists('g:neocomplete#force_omni_input_patterns')
    let g:neocomplete#force_omni_input_patterns = {}
endif

let g:jedi#popup_select_first = 0
let g:neocomplete#force_omni_input_patterns.python = '\h\w*\|[^.\t]\.\w*'
let g:jedi#goto_assignments_command = "<leader>g"
let g:jedi#goto_definitions_command = "<leader>d"
let g:jedi#documentation_command = "K"
let g:jedi#usages_command = "<leader>n"
let g:jedi#completions_command = "<C-Space>"
let g:jedi#rename_command = "<leader>r"
let g:jedi#show_call_signatures = "1"
nnoremap <Leader>l :call Flake8()<CR>
"" indentLine
let g:indentLine_color_term = 238
let g:indentLine_color_gui = '#708090'
let g:indentLine_char = '¦'

let g:syntastic_python_checkers=['flake8']

" ruby系
let g:syntastic_ruby_checkers = ['rubocop']

" golang

filetype off
filetype plugin indent off
if $GOROOT != ''
    set rtp+=$GOROOT/misc/vim/
endif
filetype plugin indent on
syntax on
autocmd FileType go autocmd BufWritePre <buffer> Fmt
exe "set rtp+=".globpath($GOPATH, "src/github.com/nsf/gocode/vim")
set completeopt=menu,preview

" C++

if executable("clang++")
    let g:syntastic_cpp_compiler = 'clang++'
    let g:syntastic_cpp_compiler_options = '--std=c++1y --stdlib=libc++'
"    let g:quickrun_config['cpp/clang+11'] = {
"                \ 'cmdopt' : '--std=c++11 --stdlib=libc++',
"                \ 'type' : 'cpp/clang++'
"                \ }
"    let g:quickrun_config['cpp'] = { 'type': 'cpp/clang++11' }
endif



call neobundle#call_hook("on_source")
    

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
" 表示関連
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
if !has('gui_running')
    set t_Co=256
endif
colorscheme hybrid
set number
set scrolloff=5
set wrap
set showbreak=+
set cursorline
set cursorcolumn
set ruler
set tabstop=8
set softtabstop=4
set expandtab
set smarttab
set laststatus=2
set foldlevel=100
" 補完ポップアップの高さ設定
set pumheight=10
syntax on


"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 検索置換
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
set incsearch
set ignorecase
set smartcase
set nohlsearch
set gdefault
if !exists('loaded_matchit')
  " matchitを有効化
  runtime macros/matchit.vim
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 入力関係
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
set backspace=indent,eol,start
set autoindent
set shiftwidth=4
set showmatch
set whichwrap=b,s,h,l,<,>,[,]
set smarttab
autocmd FileType ruby set ts=2 sw=2 softtabstop=2
autocmd FileType eruby set ts=2 sw=2 softtabstop=2

" 数字のインクリメント
nnoremap + <C-a>
nnoremap - <C-x>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" メモや一時ファイル関連
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
command! Todo edit ~/Dropbox/memo/todo.md
command! Memo edit ~/Dropbox/memo/memo.md

