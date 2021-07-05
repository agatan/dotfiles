set encoding=utf-8
scriptencoding utf8

nnoremap <Space>. :edit $MYVIMRC<CR>

" :ReloadVimrc load $MYVIMRC
command! ReloadVimrc source $MYVIMRC

" ================================================================================
" minpac {{{

packadd minpac

if !exists('g:loaded_minpac')
  " minpac not found.
  echo 'minpac not found. Run `git clone https://github.com/k-takata/minpac.git ~/.vim/pack/minpac/opt/minpac`.'
  call system("git clone https://github.com/k-takata/minpac.git ~/.vim/pack/minpac/opt/minpac")
endif

call minpac#init()

call minpac#add('cocopon/iceberg.vim')
call minpac#add('rakr/vim-one')
call minpac#add('ghifarit53/tokyonight-vim')
call minpac#add('sheerun/vim-polyglot')
call minpac#add('itchyny/lightline.vim')

call minpac#add('editorconfig/editorconfig-vim')

call minpac#add('junegunn/fzf')
call minpac#add('junegunn/fzf.vim')

call minpac#add('itchyny/vim-cursorword')

call minpac#add('thinca/vim-qfreplace')

""" {{{ coc.nvim
call minpac#add('neoclide/coc.nvim', {'branch': 'release'})
" call minpac#add('liuchengxu/vista.vim')

if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

command! -nargs=0 Prettier :CocCommand prettier.formatFile
command! -nargs=0 Format <Plug>(coc-format)
nmap <silent> fmt <Plug>(coc-format)
" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

call minpac#add('tpope/vim-fugitive')

""" }}}

call minpac#add('tyru/caw.vim')
nmap <Space>c <Plug>(caw:hatpos:toggle)
vmap <Space>c <Plug>(caw:hatpos:toggle)

call minpac#add('junegunn/vim-easy-align')

call minpac#add('tpope/vim-endwise')
call minpac#add('tpope/vim-surround')

call minpac#add('cespare/vim-toml')
call minpac#add('mrk21/yaml-vim')
call minpac#add('mattn/vim-goimports')

call minpac#add('tpope/vim-rails')

command! PackUpdate packadd minpac | source $MYVIMRC | call minpac#update('', {'do': 'call minpac#status()'})
command! PackClean  packadd minpac | source $MYVIMRC | call minpac#clean()
command! PackStatus packadd minpac | source $MYVIMRC | call minpac#status()

" }}}

" ================================================================================
" fzf {{{
command! FZFMru call fzf#run(fzf#wrap(fzf#vim#with_preview({
      \ 'source': v:oldfiles,
      \ 'sink': 'e',
      \ })))
noremap [fzf] <Nop>
nmap <Space>f [fzf]
nnoremap <silent> [fzf]f :<C-u>Files<CR>
nnoremap <silent> [fzf]m :<C-u>FZFMru<CR>
nnoremap <silent> [fzf]b :<C-u>Buffers<CR>

nnoremap <silent> * :<C-u>Lines <C-R><C-W><CR>
nnoremap <silent> <C-]> :<C-u>Tags <C-R><C-W><CR>

noremap <dev> <Nop>
map m <dev>
nnoremap <silent> <dev>f  :<C-u>call CocAction('format')<CR>
nnoremap <silent> <dev>a  :<C-u>CocAction<CR>
vnoremap <silent> <dev>a  :<C-u>CocAction<CR>
nnoremap <silent> <dev>q  :<C-u>CocCommand fzf-preview.CocCurrentDiagnostics<CR>
nnoremap <silent> <dev>Q  :<C-u>CocCommand fzf-preview.CocDiagnostics<CR>
nnoremap <silent> <dev>rf :<C-u>CocCommand fzf-preview.CocReferences<CR>
nnoremap <silent> <dev>t  :<C-u>CocCommand fzf-preview.CocTypeDefinitions<CR>
nnoremap <silent> <Space>e :<C-u>CocCommand explorer<CR>

let g:fzf_buffers_jump = 1
let g:fzf_commits_log_options = '--graph --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr"'
let g:fzf_layout = { 'window': { 'width': 0.8, 'height': 0.8, 'highlight': 'Todo', 'border': 'sharp' } }
let $FZF_PREVIEW_PREVIEW_BAT_THEME = 'gruvbox'
" }}}


" ================================================================================
" Language {{{
augroup myvimrc
  au!
  autocmd FileType ruby setlocal regexpengine=1
  autocmd FileType crystal setlocal regexpengine=1
augroup END
" }}}
"

" ================================================================================
" Vim builtin settings {{{

filetype plugin indent on
syntax enable

if has('termguicolors')
    set termguicolors
endif

set background=dark
" let g:tokyonight_enable_italic = 1
let g:tokyonight_disable_italic_comment = 1
colorscheme tokyonight

let g:lightline = {
   \ 'colorscheme': 'tokyonight',
   \   'active': {
   \     'left': [ [ 'mode', 'paste' ],
   \               ['readonly', 'filename', 'modified' ] ]
   \   },
   \ }


set incsearch
set ignorecase
set smartcase
if !exists('loaded_matchit')
  " matchitを有効化
  runtime macros/matchit.vim
endif

augroup grepquickfix
  autocmd!
  autocmd QuickFixCmdPost *grep* cwindow
augroup END

let g:mapleader = ','
set updatetime=300
set clipboard=unnamed,unnamedplus
set hidden
set noshowmode
set number
set signcolumn=number
set scrolloff=5
set cmdheight=2
set wildmode=list:longest
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

"" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

nnoremap ; :
vnoremap ; :
nnoremap : ;
vnoremap : ;
nnoremap q; q:
vnoremap q; q:

" }}}


" F#, fsharp
autocmd BufNewFile,BufRead *.fs,*.fsi,*.fsx set filetype=fsharp
