scriptencoding=utf-8
" vim:set ts=8 sts=2 sw=2 tw=0:
" --------------------------------------------------------------------------

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

set shiftround
set infercase
set virtualedit=all
set hidden
set switchbuf=useopen
set matchtime=3

"---------------------------------------------------------------------------
set number
set ruler
set list
set listchars=tab:>-,trail:-,extends:<
set nowrap
set laststatus=2
set cmdheight=2
set showcmd
set title
colorscheme default

set foldmethod=marker

set nobackup
set noswapfile
set noundofile

augroup MyAutoCmd
  autocmd!
augroup END

if !has('gui_running')
  set t_Co=256
endif

let mapleader = ","

if has('vim_starting')
  set nocompatible
  set runtimepath+=~/.vim/bundle/neobundle.vim/
  set runtimepath+=$GOROOT/misc/vim
  set runtimepath+=$GOPATH/src/github.com/nsf/gocode/vim
endif


filetype plugin indent off

function! s:yaml_load(filename)
ruby << EOF
  require 'yaml'
  obj = YAML.load_file(File.expand_path(VIM::evaluate('a:filename')))
  obj_hash = obj.inspect.gsub('=>', ':').gsub('nil', '{}')
  VIM::command("let l:ret = '#{obj_hash}'")
EOF
  return l:ret
endfunction


" Required:
call neobundle#begin(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

let s:my_bundles = s:yaml_load(expand('~/.vim/bundles.yml'))

for bundle in eval(s:my_bundles)
  for [bundle_source_name, bundle_command_dict] in items(bundle)
    " echo '------------------------------'
    let s:command_pair = items(bundle_command_dict)
    let s:bundle_command = s:command_pair[0][0]
    let s:bundle_options = s:command_pair[0][1]
    " echo bundle_source_name
    " echo s:bundle_command
    " echo s:bundle_options
    execute s:bundle_command . ' "' . bundle_source_name . '", ' . string(s:bundle_options)
  endfor
endfor

let g:neocomplete#enable_at_startup = 1
let s:hooks = neobundle#get_hooks("neocomplete.vim")
function! s:hooks.on_source(bundle)
  let g:acp_enableAtStartup = 0
  let g:neocomplet#enable_smart_case = 1
endfunction


nnoremap [unite] <Nop>
nmap U [unite]
nnoremap <silent> [unite]f :<C-u>UniteWithBufferDir -buffer-name=files file<CR>
nnoremap <silent> [unite]b :<C-u>Unite buffer<CR>
nnoremap <silent> [unite]r :<C-u>Unite register<CR>
nnoremap <silent> [unite]m :<C-u>Unite file_mru<CR>
nnoremap <silent> [unite]c :<C-u>Unite bookmark<CR>
nnoremap <silent> [unite]o :<C-u>Unite outline<CR>
nnoremap <silent> [unite]t :<C-u>Unite tab<CR>
nnoremap <silent> [unite]w :<C-u>Unite window<CR>
let s:hooks = neobundle#get_hooks("unite.vim")
function! s:hooks.on_source(bundle)
  " start unite in insert mode
  let g:unite_enable_start_insert = 1
  " use vimfiler to open directory
  call unite#custom_default_action("source/bookmark/directory", "vimfiler")
  call unite#custom_default_action("directory", "vimfiler")
  call unite#custom_default_action("directory_mru", "vimfiler")
  autocmd MyAutoCmd FileType unite call s:unite_settings()
  function! s:unite_settings()
    imap <buffer> <Esc><Esc> <Plug>(unite_exit)
    nmap <buffer> <Esc> <Plug>(unite_exit)
    nmap <buffer> <C-n> <Plug>(unite_select_next_line)
    nmap <buffer> <C-p> <Plug>(unite_select_previous_line)
  endfunction
endfunction

nnoremap <Leader>e :VimFilerExplorer<CR>

" close vimfiler automatically when there are only vimfiler open
autocmd MyAutoCmd BufEnter * if (winnr('$') == 1 && &filetype ==# 'vimfiler') | q | endif
let s:hooks = neobundle#get_hooks("vimfiler")
function! s:hooks.on_source(bundle)
  let g:vimfiler_as_default_explorer = 1
  let g:vimfiler_enable_auto_cd = 1

  let g:vimfiler_ignore_pattern = "\%(^\..*\|\.pyc$\)"

  " vimfiler specific key mappings
  autocmd MyAutoCmd FileType vimfiler call s:vimfiler_settings()
  function! s:vimfiler_settings()
    " ^^ to go up
    nmap <buffer> ^^ <Plug>(vimfiler_switch_to_parent_directory)
    " use R to refresh
    nmap <buffer> R <Plug>(vimfiler_redraw_screen)
    " overwrite C-l
    nmap <buffer> <C-l> <C-w>l
  endfunction
endfunction


nmap <Leader>r <Plug>(quickrun)
let s:hooks = neobundle#get_hooks("vim-quickrun")
function! s:hooks.on_source(bundle)
  let g:quickrun_config = {
        \ "_": {"runner": "remote/vimproc"},
        \ "cpp/clang++" : {
        \   "hook/time/enable" : 1
        \ },
        \ "cc/clang" : {
        \   "hook/time/enable" : 1
        \ }
        \}
endfunction


let s:hooks = neobundle#get_hooks("jedi-vim")
function! s:hooks.on_source(bundle)
  setlocal omnifunc=jedi#completions
  "let g:jedi#popup_select_first=0
  let g:jedi#completions_enabled = 0
  " let g:neocomplete#force_omni_input_patterns.python = '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'
  let g:jedi#auto_vim_configuration = 0
  let g:jedi#rename_command = '<Leader>R'
  let g:jedi#goto_assignments_command = '<Leader>G'
  NeoCompleteEnable
endfunction


function! s:golang_init()
  let g:syntastic_go_checkers = ['go', 'golint']
  nmap <Leader>t :TagbarToggle<CR>
  let g:tagbar_type_go = {
        \ 'ctagstype' : 'go',
        \ 'kinds'     : [
        \   'p:package',
        \   'i:imports:1',
        \   'c:constants',
        \   'v:variables',
        \   't:types',
        \   'n:interfaces',
        \   'w:fields',
        \   'e:embedded',
        \   'm:methods',
        \   'r:constructor',
        \   'f:functions'
        \ ],
        \ 'sro' : '.',
        \ 'kind2scope' : {
        \   't' : 'ctype',
        \   'n' : 'ntype'
        \ },
        \ 'scope2kind' : {
        \   'ctype' : 't',
        \   'ntype' : 'n'
        \ },
        \ 'ctagsbin'  : 'gotags',
        \ 'ctagsargs' : '-sort -silent'
        \ }
  NeoCompleteEnable
endfunction
autocmd MyAutoCmd FileType go call s:golang_init()


" --------------------------------------------------------------------------------
function! s:ruby_init()
  setl sw=2
  setl sts=2
  setl ts=2
  setl expandtab
  NeoCompleteEnable
endfunction
autocmd MyAutoCmd FileType ruby call s:ruby_init()


" --------------------------------------------------------------------------------
function! s:haskell_init()
  " let g:haddock_docdir="C:/Apps/HaskellPlatform/2013.2.0.0/doc/html"
  " let g:haddock_browser="chrome.exe"
  " let g:ghc="ghc"
  " compiler ghc
  setl sw=2
  setl sts=2
  setl ts=2
  setl expandtab
  NeoCompleteEnable
endfunction
autocmd MyAutoCmd FileType haskell call s:haskell_init()


call neobundle#end()

" Required:
filetype plugin indent on

NeoBundleCheck

if has('unnamedplus')
  set clipboard& clipboard+=unnamedplus,unnamed
else
  set clipboard& clipboard+=unnamed
endif

"---------------------------------------------------------------------------
function! CommentMark(docomment, a, b)
  if !exists('b:comment')
    let b:comment = CommentStr() . ' '
  endif
  if a:docomment
    exe "normal! '" . a:a . "_\<C-V>'" . a:b . 'I' . b:comment
  else
    exe "'".a:a.",'".a:b . 's/^\(\s*\)' . escape(b:comment,'/') . '/\1/e'
  endif
endfunction

" Comment lines in marks set by g@ operator.
function! DoCommentOp(type)
  call CommentMark(1, '[', ']')
endfunction

" Uncomment lines in marks set by g@ operator.
"
function! UnCommentOp(type)
  call CommentMark(0, '[', ']')
endfunction

" Return string used to comment line for current filetype.
function! CommentStr()
  if count(['c', 'cpp', 'java', 'scala', 'javascript', 'go'], &ft)
    return '//'
  elseif count(['vim'], &ft)
    return '"'
  elseif count(['python', 'perl', 'sh', 'R', 'ruby'], &ft)
    return '#'
  elseif count(['haskell'], &ft)
    return '--'
  elseif count(['lisp'], &ft)
    return ';'
  endif
  return ''
endfunction

nnoremap <Leader>c <Esc>:set opfunc=DoCommentOp<CR>g@
nnoremap <Leader>C <Esc>:set opfunc=UnCommentOp<CR>g@
vnoremap <Leader>c <Esc>:call CommentMark(1,'<','>')<CR>
vnoremap <Leader>C <Esc>:call CommentMark(0,'<','>')<CR>
