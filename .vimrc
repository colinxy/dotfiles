" 256 color
set t_Co=256

set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}
Plugin 'tpope/vim-fugitive'

call vundle#end()
filetype plugin indent on


" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just
" :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to
" auto-approve removal
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line


" powerline
" set guifont=DejaVu\ Sans\ Mono\ for\ Powerline\ 9
set laststatus=2
" let g:Powerline_symbols = 'fancy'


imap jk <Esc>
set tm=300


" set number

" set showcmd


" colorscheme pablo
syntax on

" indentation
set autoindent
filetype plugin indent on

" 4 space tab and use softtab
set tabstop=4
set shiftwidth=4
set expandtab

" incremental highlight search
set incsearch
set hlsearch

" set foldenble
