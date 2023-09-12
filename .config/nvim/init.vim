set encoding=utf-8
scriptencoding utf-8

nnoremap <Space>. :edit $MYVIMRC<CR>

" :ReloadVimrc load $MYVIMRC
command! ReloadVimrc source $MYVIMRC

let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

filetype plugin indent on
syntax enable


" {{{ Plugins
call plug#begin(stdpath('data') . '/plugged')

" Basis
Plug 'vim-denops/denops.vim'

" View
Plug 'cocopon/iceberg.vim'

" Plug 'nvim-treesitter/nvim-treesitter', { 'do': ':TSUpdate' }
Plug 'itchyny/lightline.vim'
" Plug 'itchyny/vim-cursorword'

" Edit
Plug 'junegunn/vim-easy-align'
Plug 'editorconfig/editorconfig-vim'
Plug 'tyru/caw.vim'
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'nvim-lua/completion-nvim'

" Language Server (LSP)
Plug 'neovim/nvim-lspconfig'
Plug 'williamboman/mason.nvim'
Plug 'williamboman/mason-lspconfig.nvim'
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/vim-vsnip'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-cmdline'
Plug 'ibhagwan/fzf-lua', {'branch': 'main'}
Plug 'kyazdani42/nvim-web-devicons'

Plug 'vim-denops/denops.vim'
Plug 'hashivim/vim-terraform'
Plug 'jparise/vim-graphql'
Plug 'udalov/kotlin-vim'

call plug#end()

" Plugin Configurations
" LSP
lua <<EOS
-- 1. LSP Sever management
require('mason').setup()
require('mason-lspconfig').setup_handlers({ function(server)
  local opt = {
    -- -- Function executed when the LSP server startup
    -- on_attach = function(client, bufnr)
    --   local opts = { noremap=true, silent=true }
    --   vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
    --   vim.cmd 'autocmd BufWritePre * lua vim.lsp.buf.formatting_sync(nil, 1000)'
    -- end,
    capabilities = require('cmp_nvim_lsp').default_capabilities()
  }
  require('lspconfig')[server].setup(opt)
end })

-- 2. build-in LSP function
-- keyboard shortcut
vim.keymap.set('n', 'K',  '<cmd>lua vim.lsp.buf.hover()<CR>')
vim.keymap.set('n', 'gf', '<cmd>lua vim.lsp.buf.formatting()<CR>')
vim.keymap.set('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>')
vim.keymap.set('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>')
vim.keymap.set('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>')
vim.keymap.set('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>')
vim.keymap.set('n', 'gt', '<cmd>lua vim.lsp.buf.type_definition()<CR>')
vim.keymap.set('n', 'gn', '<cmd>lua vim.lsp.buf.rename()<CR>')
vim.keymap.set('n', 'ga', '<cmd>lua vim.lsp.buf.code_action()<CR>')
vim.keymap.set('n', 'ge', '<cmd>lua vim.diagnostic.open_float()<CR>')
vim.keymap.set('n', 'g]', '<cmd>lua vim.diagnostic.goto_next()<CR>')
vim.keymap.set('n', 'g[', '<cmd>lua vim.diagnostic.goto_prev()<CR>')
-- LSP handlers
vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, { virtual_text = false }
)
-- Reference highlight
vim.cmd [[
set updatetime=500
highlight LspReferenceText  cterm=underline ctermfg=1 ctermbg=8 gui=underline guifg=#A00000 guibg=#104040
highlight LspReferenceRead  cterm=underline ctermfg=1 ctermbg=8 gui=underline guifg=#A00000 guibg=#104040
highlight LspReferenceWrite cterm=underline ctermfg=1 ctermbg=8 gui=underline guifg=#A00000 guibg=#104040
augroup lsp_document_highlight
  autocmd!
  autocmd CursorHold,CursorHoldI *.rs lua vim.lsp.buf.document_highlight()
  autocmd CursorMoved,CursorMovedI *.rs lua vim.lsp.buf.clear_references()
augroup END
]]

-- 3. completion (hrsh7th/nvim-cmp)
local cmp = require("cmp")
cmp.setup({
  snippet = {
    expand = function(args)
      vim.fn["vsnip#anonymous"](args.body)
    end,
  },
  sources = {
    { name = "nvim_lsp" },
    { name = "buffer" },
    { name = "path" },
  },
  mapping = cmp.mapping.preset.insert({
    ["<C-p>"] = cmp.mapping.select_prev_item(),
    ["<C-n>"] = cmp.mapping.select_next_item(),
    ['<C-l>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<C-e>'] = cmp.mapping.abort(),
    ['<Tab>'] = function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      else
        fallback()
      end
    end,
    ['<S-Tab>'] = function(fallback)
      if cmp.visivle() then
        cmp.select_prev_item()
      else
        fallback()
      end
    end,
  }),
  experimental = {
    ghost_text = true,
  },
})
EOS

" Use <Tab> and <S-Tab> to navigate through popup menu
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"


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

let g:fzf_buffers_jump = 1
let g:fzf_commits_log_options = '--graph --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr"'
let g:fzf_layout = { 'window': { 'width': 0.8, 'height': 0.8, 'highlight': 'Todo', 'border': 'sharp' } }
let $FZF_PREVIEW_PREVIEW_BAT_THEME = 'gruvbox'
" }}}

nmap <space>c <Plug>(caw:hatpos:toggle)
vmap <space>c <Plug>(caw:hatpos:toggle)

" }}}

set background=dark
colorscheme iceberg

set showcmd
set incsearch
set ignorecase
set smartcase
let g:mapleader = ','
set inccommand=split
set lazyredraw
set updatetime=300
set clipboard=unnamed,unnamedplus
set hidden
set noshowmode
set number
set signcolumn=number
set scrolloff=5
set cmdheight=1
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
set shortmess+=c
set cursorline


nnoremap ; :
vnoremap ; :
nnoremap : ;
vnoremap : ;
nnoremap q; q:
vnoremap q; q:


" Windows Subsystem for Linux で、ヤンクでクリップボードにコピー
" https://oki2a24.com/2019/04/04/yank-to-clipbord-in-wsl-vim/
if system('uname -a | grep microsoft') != ''
  augroup myYank
    autocmd!
    autocmd TextYankPost * :call system('clip.exe', @")
  augroup END
endif

" Quickfix
let &grepprg = 'rg --no-heading --hidden --no-ignore -g "!.git/" --color never --json $* \| jq -r ''select(.type=="match")\|.data as $data\|$data.submatches[]\|"\($data.path.text):\($data.line_number):\(.start+1):\(.end+1):\($data.lines.text//""\|sub("\n$";""))"'''
set grepformat=%f:%l:%c:%k:%m
autocmd! QuickfixCmdPost make,grep,grepadd,vimgrep cwindow
nnoremap <silent> <C-p> :<C-u>cp<CR>
nnoremap <silent> <C-n> :<C-u>cn<CR>
function! ToggleQuickfix()
    let l:nr = winnr('$')
    cwindow
    let l:nr2 = winnr('$')
    if l:nr == l:nr2
        cclose
    endif
endfunction
nnoremap <script> <silent> qf :call ToggleQuickfix()<CR>
