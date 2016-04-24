" vim:set ts=8 sts=2 sw=2 tw=0:
" --------------------------------------------------------------------------

let mapleader = " "

call plug#begin('~/.config/nvim/plugged')

Plug 'Shougo/vimproc.vim', { 'do': 'make' }

Plug 'trusktr/seti.vim'

Plug 'luochen1990/rainbow'
let g:rainbow_active = 1

Plug 'kovisoft/paredit', { 'for': ['clojure', 'scheme'] }

Plug 'itchyny/lightline.vim'
let g:lightline = {
      \ 'colorscheme': 'hybrid',
      \ 'active': {
      \   'left': [
      \     [ 'mode', 'paste' ],
      \     [ 'readonly', 'modified' ],
      \   ],
      \   'right': [
      \     [ 'lineinfo' ],
      \     [ 'qfstatusline', 'fileformat', 'fileencoding', 'filetype' ],
      \   ],
      \ },
      \ 'inactive': {
      \   'left': [
      \     [ 'filename', 'readonly', 'modified' ],
      \   ],
      \   'right': [
      \     [ 'fileformat', 'fileencoding', 'filetype' ],
      \   ],
      \ },
      \ 'tabline': {
      \   'left': [
      \     [ 'tabs' ],
      \   ],
      \   'right': [
      \     [ 'close' ],
      \     [ 'git_branch', 'git_traffic', 'git_status', 'cwd' ],
      \   ],
      \ },
      \ 'component': {
      \   'readonly': '%{&filetype=="help"?"":&readonly?"":""}',
      \   'modified': '%{&filetype=="help"?"":&modified?"+":&modifiable?"":"-"}',
      \ },
      \ 'component_visible_condition': {
      \   'lineinfo': '(winwidth(0) >= 70)',
      \   'readonly': '(&filetype!="help"&& &readonly)',
      \   'modified': '(&filetype!="help"&&(&modified||!&modifiable))',
      \ },
      \ 'component_function': {
      \   'git_branch': 'g:lightline.my.git_branch',
      \   'git_traffic': 'g:lightline.my.git_traffic',
      \   'git_status': 'g:lightline.my.git_status',
      \ },
      \ 'component_expand': {
      \   'qfstatusline': 'qfstatusline#Update',
      \ },
      \ 'component_type': {
      \   'qfstatusline': 'error',
      \ },
      \ 'separator': { 'left': '', 'right': '' },
      \ 'subseparator': { 'left': '', 'right': '' }
      \ }

let g:lightline.my = {}
function! g:lightline.my.git_branch()
  return winwidth(0) > 70 ? gita#statusline#preset('branch') : ''
endfunction
function! g:lightline.my.git_traffic()
  return winwidth(0) > 70 ? gita#statusline#preset('traffic') : ''
endfunction
function! g:lightline.my.git_status()
  return winwidth(0) > 70 ? gita#statusline#preset('status') : ''
endfunction

" --------------------------------------------------------------------------

Plug 'scrooloose/nerdtree'
noremap <Leader>w :<C-u>NERDTreeToggle<CR>

Plug 'lambdalisue/vim-gita'

Plug 'tpope/vim-fugitive'

Plug 'gregsexton/gitv'
noremap <Leader>g :<C-u>Gitv<CR>

Plug 'airblade/vim-gitgutter'

Plug 'ctrlpvim/ctrlp.vim'
nnoremap [ctrlp] <Nop>
nmap <Leader>p [ctrlp]
nnoremap <silent> [ctrlp]p :<C-u>CtrlP<CR>
nnoremap <silent> [ctrlp]b :<C-u>CtrlPBuffer<CR>
function! s:CallCtrlPBasedOnGitStatus()
  if exists('g:ctrlp_user_command')
    unlet g:ctrlp_user_command
  endif
  let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard', 'find %s -type f']
  execute 'CtrlP'
endfunction
nnoremap <silent> [ctrlp]l :call <SID>CallCtrlPBasedOnGitStatus()<CR>

Plug 'mattn/ctrlp-ghq'
nnoremap <silent> [ctrlp]g :<c-u>CtrlPGhq<cr>
let g:ctrlp_ghq_actions = [
      \   { 'label': 'Open', 'action': 'e', 'path': 1 },
      \   { 'label': 'Look', 'action': '!ghq look', 'path': 0 },
      \ ]

Plug 'Konfekt/FastFold'

Plug 'Shougo/deoplete.nvim'
let g:deoplete#enable_at_startup = 1

Plug 'thinca/vim-quickrun'
if !exists('g:quickrun_config')
  let g:quickrun_config = {}
endif
let g:quickrun_config['watchdogs_checker/_'] = {
      \   'runner': 'vimproc',
      \   'runner/vimproc/updatetime' : 60,
      \   'outputter': 'error',
      \   'outputter/error/success': 'buffer',
      \   'outputter/error/error': 'quickfix',
      \   'outputter/buffer/split': ':rightbelow 8sp',
      \   'outputter/buffer/close_on_empty' : 1,
      \   'outputter/quickfix/open_cmd' : '',
      \   'hook/qfstatusline_update/enable_exit' : 1,
      \   'hook/qfstatusline_update/priority_exit' : 4,
      \ }

Plug 'jceb/vim-hier'

Plug 'dannyob/quickfixstatus'

Plug 'osyo-manga/shabadou.vim'

Plug 'osyo-manga/vim-watchdogs'

Plug 'KazuakiM/vim-qfstatusline'
let g:Qfstatusline#UpdateCmd = function('lightline#update')

Plug 'easymotion/vim-easymotion'
nnoremap [em] <Nop>
nmap <Leader>e [em]
map  [em]/ <Plug>(easymotion-sn)
omap [em]/ <Plug>(easymotion-tn)
" map  n <Plug>(easymotion-next)
" map  N <Plug>(easymotion-prev)

Plug 'majutsushi/tagbar'
nmap <Leader>o :TagbarToggle<CR>
let g:tagbar_autofocus = 1

Plug 'tpope/vim-commentary'

Plug 'vim-scripts/gtags.vim'
" ,gでタグファイルを生成する
nnoremap <Leader>v :!gtags\ --gtagslabel=pygments<CR>
nnoremap <C-g> :Gtags
" カレントファイル内の関数一覧
nnoremap <C-l> :Gtags -f %<CR>
" カーソル上の関数の定義場所へジャンプ
nnoremap <C-j> :GtagsCursor<CR>
vnoremap <C-j> :GtagsCursor<CR>
" Usagesを表示
nnoremap <C-h> :Gtags -r <C-r><C-w><CR>
vnoremap <C-h> :Gtags -r <C-r><C-w><CR>

"---------------------------------------------------------------------------

" --------------------------------------------------
"
Plug 'dag/vim2hs', { 'for' : 'haskell' }

" Plug 'bitc/lushtags'
" let g:tagbar_type_haskell = {
"     \ 'ctagsbin' : 'lushtags',
"     \ 'ctagsargs' : '--ignore-parse-error --',
"     \ 'kinds' : [
"         \ 'm:module:0',
"         \ 'e:exports:1',
"         \ 'i:imports:1',
"         \ 't:declarations:0',
"         \ 'd:declarations:1',
"         \ 'n:declarations:1',
"         \ 'f:functions:0',
"         \ 'c:constructors:0'
"     \ ],
"     \ 'sro' : '.',
"     \ 'kind2scope' : {
"         \ 'd' : 'data',
"         \ 'n' : 'newtype',
"         \ 'c' : 'constructor',
"         \ 't' : 'type'
"     \ },
"     \ 'scope2kind' : {
"         \ 'data' : 'd',
"         \ 'newtype' : 'n',
"         \ 'constructor' : 'c',
"         \ 'type' : 't'
"     \ }
" \ }

" Plug 'neovimhaskell/nvim-hs', { 'for' : 'haskell', 'do' : 'stack build && stack install && cp -p nvim-hs-devel.sh ~/.local/bin/' }
" if has('nvim') " This way you can also put it in your vim config file
"   call rpcrequest(rpcstart(expand('$HOME/.local/bin/nvim-hs-devel.sh')), "PingNvimhs")
" endif

" Plug 'neovimhaskell/haskell-vim', { 'for' : 'haskell' }

Plug 'eagletmt/neco-ghc', { 'for' : 'haskell' }
let g:haskellmode_completion_ghc = 0

" Plug 'neovimhaskell/neovim-ghcmod', { 'for' : 'haskell' }

" Plug 'pbrisbin/vim-syntax-shakespeare', { 'for' : 'haskell' }

Plug 'tpope/vim-fireplace', { 'for': 'clojure' }

Plug 'lambdatoast/elm.vim', { 'for' : 'elm' }

Plug 'raichoo/purescript-vim', { 'for' : 'purescript' }

Plug 'derekwyatt/vim-sbt', { 'for' : 'sbt.scala' }

Plug 'derekwyatt/vim-scala', { 'for' : 'scala' }

Plug 'vim-ruby/vim-ruby', { 'for' : 'ruby' }

Plug 'lambdalisue/vim-pyenv', { 'for' : 'python' }

Plug 'davidhalter/jedi-vim', { 'for' : 'python' }
" let g:jedi#auto_initialization = 0
" let g:jedi#auto_vim_configuration = 0
" let g:jedi#use_tabs_not_buffers = 1
" let g:jedi#use_splits_not_buffers = "left"
" let g:jedi#popup_on_dot = 0
" let g:jedi#popup_select_first = 0
let g:jedi#show_call_signatures = "1"
let g:jedi#goto_command = "<Leader>d"
let g:jedi#goto_assignments_command = "<Leader>g"
let g:jedi#goto_definitions_command = "gd"
let g:jedi#documentation_command = "K"
let g:jedi#usages_command = "<Leader>n"
let g:jedi#completions_command = "<C-Space>"
let g:jedi#rename_command = "<Leader>r"

Plug 'StanAngeloff/php.vim', { 'for' : 'php' }

Plug 'fatih/vim-go'
let g:go_fmt_commands='goimports'
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1

Plug 'artur-shaik/vim-javacomplete2', { 'for' : 'java' }

Plug 'othree/html5.vim'

Plug 'tpope/vim-surround'


call plug#end()

"---------------------------------------------------------------------------

nnoremap [buffer] <Nop>
nmap <Leader>b [buffer]
nnoremap <silent> [buffer]n :<C-u>bn<CR>
nnoremap <silent> [buffer]p :<C-u>bp<CR>
nnoremap <silent> [buffer]l :<C-u>ls<CR>

if has('unnamedplus')
  set clipboard& clipboard+=unnamedplus,unnamed
else
  set clipboard& clipboard+=unnamed
endif

"---------------------------------------------------------------------------

colorschem seti

set colorcolumn=80
set cursorline
set tabstop=8
set expandtab
set autoindent
set backspace=indent,eol,start
set wrapscan
set showmatch
set wildmenu
set formatoptions+=mM
set matchpairs& matchpairs+=<:>

set relativenumber
set nostartofline
set shiftround
set infercase
set virtualedit=all
set switchbuf=useopen
set matchtime=3

"---------------------------------------------------------------------------
set number
set ruler
set list
set listchars=tab:▸\ ,trail:-,extends:<
set wrap
set showtabline=2
set laststatus=2
set cmdheight=2
set showcmd
set title
set hlsearch

" set foldmethod=marker
set foldmethod=syntax
set foldlevel=99

set noerrorbells
set nobackup
set noswapfile
set noundofile
set hidden

"---------------------------------------------------------------------------
"
augroup MyAuGroup
  autocmd!
augroup END

function! s:golang_init()
  setl sw=4 sts=4 ts=4 noet
  nmap <Leader>i <Plug>(go-info)
  nmap <Leader>s <Plug>(go-implements)
  nmap <Leader>gb <Plug>(go-doc-browser)
  nmap <Leader>gd <Plug>(go-doc)
  nmap <Leader>gv <Plug>(go-doc-vertical)
  nmap <Leader>ds <Plug>(go-def-split)
  nmap <Leader>dv <Plug>(go-def-vertical)
  nmap <Leader>dt <Plug>(go-def-tab)
  nmap <leader>r <Plug>(go-run)
  nmap <leader>b <Plug>(go-build)
  nmap <leader>t <Plug>(go-test)
  nmap <leader>c <Plug>(go-coverage)
endfunction
autocmd MyAuGroup FileType go call s:golang_init()

function! s:haskell_init()
  setl sw=4 sts=4 ts=8 et sr
  setl formatprg=pointfree
  setl omnifunc=necoghc#omnifunc
  " autocmd MyAuGroup BufWritePost <buffer> GhcModCheckAsync
endfunction
autocmd MyAuGroup FileType haskell call s:haskell_init()

function! s:php_init()
  setl sw=4 sts=4 ts=4 noet
endfunction
autocmd MyAuGroup FileType php call s:php_init()

function! s:ruby_init()
  setl sw=2 sts=2 ts=2 et
endfunction
autocmd MyAuGroup FileType ruby call s:ruby_init()

function! s:scala_init()
  setl sw=2 sts=2 ts=2 et
endfunction
autocmd MyAuGroup FileType scala call s:scala_init()

function! s:perl_init()
  setl sw=2 sts=2 ts=2 et
endfunction
autocmd MyAuGroup FileType perl call s:perl_init()

function! s:java_init()
  set ts=4 sw=4 sts=0 et
  nnoremap [java] <Nop>
  nmap <Leader>j [java]
  nnoremap <silent> [java]i <Plug>(JavaComplete-Imports-Add)
  nnoremap <silent> [java]d <Plug>(JavaComplete-Imports-AddMissing)
  nnoremap <silent> [java]r <Plug>(JavaComplete-Imports-RemoveUnused)
endfunction
autocmd MyAuGroup FileType java call s:java_init()

function! s:purs_init()
  set ts=2 sw=2 sts=0 et
  autocmd InsertLeave,BufWritePost,TextChanged *.py WatchdogsRun
  autocmd BufRead,BufNewFile *.py WatchdogsRun
endfunction
autocmd MyAuGroup FileType purescript call s:purs_init()

autocmd MyAuGroup FileType qf nnoremap <silent><buffer>q :q<CR>
autocmd MyAuGroup FileType help nnoremap <silent><buffer>q <C-w>c
