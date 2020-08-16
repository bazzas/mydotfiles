" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other optios as a side effect.
set nocompatible

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'
" Plugin 'mlent/ale' -- Has a small change for multi-line ghc errors, see below
Plugin 'w0rp/ale'
Plugin 'vim-airline/vim-airline'
Plugin 'eagletmt/ghcmod-vim'
Plugin 'Shougo/vimproc'
Plugin 'ap/vim-css-color'
Plugin 'scrooloose/nerdtree'
Plugin 'junegunn/fzf'
Plugin 'junegunn/fzf.vim'
Plugin 'Shougo/vimproc.vim'
Plugin 'easymotion/vim-easymotion'
call vundle#end()

let g:airline#extensions#ale#enabled = 1

"nnoremap <SPACE> <Nop>
let mapleader="\<Space>"



nnoremap <Leader>ht :GhcModType<cr>
nnoremap <Leader>htc :GhcModTypeClear<cr>

set relativenumber 

inoremap df <Esc>

" autocmd vimenter * NERDTree "
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

"Fuzzy file finder"
let g:fzf_preview_window = 'right:60%'
nnoremap <c-p> :Files<cr>
nnoremap <c-f> :BLines<cr>
augroup fzf
  autocmd!
  autocmd! FileType fzf
  autocmd  FileType fzf set laststatus=0 noshowmode noruler
    \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler
augroup END

" Nerdtree "
nnoremap <Leader>f :NERDTreeToggle<Enter>
" Autoclose nerd tree when open a file "
let NERDTreeQuitOnOpen = 1
" automatically close tab if the only remaining window is NerdTree "
"autocmd bufenter * if (winnr(“$”) == 1 && exists(“b:NERDTreeType”) && b:NERDTreeType == “primary”) | q | endif

set backspace=indent,eol,start  "Allow backspace in insert mode
set history=1000                "Store lots of :cmdline history

" ================ Turn Off Swap Files ==============

set noswapfile
set nobackup
set nowb


" ================ Persistent Undo ==================
" Keep undo history across sessions, by storing in file.
" Only works all the time.
if has('persistent_undo')
  silent !mkdir ~/.vim/backups > /dev/null 2>&1
  set undodir=~/.vim/backups
  set undofile
endif

" ================ Indentation ======================

set autoindent
set smartindent
set smarttab
set shiftwidth=2
set softtabstop=2
set tabstop=2
set expandtab

" Auto indent pasted text
nnoremap p p=`]<C-o>
nnoremap P P=`]<C-o>
