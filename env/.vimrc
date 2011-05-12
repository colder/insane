syn on
set ai
set nosi
set nocin
set inde=""
set sta
set et
set ts=4
set sw=4
set softtabstop=4
set guifont=courier
set bsk=*
set foldmethod=marker
set whichwrap+=<,>,h,l
set lcs=tab:>-,trail:.
set list
set tabpagemax=50
set ruler

set ignorecase
set smartcase
set sessionoptions=blank,buffers,curdir,folds,help,options,tabpages,winsize,winpos

filetype indent plugin on

hi Normal ctermbg=black
hi Comment ctermfg=7
hi Search ctermbg=white

map <F2> <Esc>:BufExplorer<CR>
nmap <C-a> <Esc>

map <F3> <Esc>:TlistToggle<CR>
imap <F3> <Esc>:TlistToggle<CR>
map <F4> <Esc>:TlistToggle<CR>
imap <F4> <Esc>:TlistToggle<CR>
"
"source ~/.vim/minibufexpl.vim
"let g:miniBufExplMapWindowNavVim = 1
"let g:miniBufExplMapWindowNavArrows = 1
"let g:miniBufExplMapCTabSwitchBufs = 1
"let g:miniBufExplModSelTarget = 1 

"map <F3> <Esc>gT<CR>
"imap <F3> <Esc>gT<CR>
"map <F4> <Esc>gt<CR>
"imap <F4> <Esc>gt<CR>

command W :w
command Wq :wq
command Wqa :wqa

command Wsudo :w !sudo tee % > /dev/null

augroup filetype
  au BufRead,BufNewFile *.flex,*.jflex    set filetype=jflex
  au BufRead,BufNewFile *.cup    set filetype=cup
augroup END
au Syntax jflex    so ~/.vim/syntax/jflex.vim

au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
"cabbrev edit tabedit
set autowrite
set showcmd
set showmode
set hidden
set wildmenu
set wildmode=longest,list
set ignorecase
set smartcase
set ruler
set scrolloff=3
set title

"au BufWritePre * set et | retab

