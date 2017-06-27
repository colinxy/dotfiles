" 256 color
set t_Co=256

set background=dark

set nocompatible
filetype off

" Vundle setup
" git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}
Plugin 'tpope/vim-fugitive'
Plugin 'scrooloose/nerdtree'

call vundle#end()

" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just
" :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to
" auto-approve removal
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line


" nerdtree
map <C-n> :NERDTreeToggle<CR>

" powerline
" set guifont=DejaVu\ Sans\ Mono\ for\ Powerline\ 9
set laststatus=2
" let g:Powerline_symbols = 'fancy'


imap jk <Esc>
set tm=500
nmap <C-v> :vsplit<CR>
nmap <C-l> :wincmd l<CR>
nmap <C-h> :wincmd h<CR>
set mouse=nicr

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
set ignorecase
set smartcase
set nowrapscan

" Allow backspacing over everything in insert mode.
set backspace=indent,eol,start

" set foldenable

" keep last position
autocmd BufReadPost *
  \ if line("'\"") >= 1 && line("'\"") <= line("$") |
  \   exe "normal! g`\"" |
  \ endif
