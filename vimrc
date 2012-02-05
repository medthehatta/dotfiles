set autoindent
set number
set incsearch
set hlsearch
nnoremap <Space> :set hlsearch!<CR>
noremap ; :
noremap , ;
nnoremap ,c :s/^/# /<CR>
nnoremap ,u :s/^# //<CR>
imap jj <esc>
set cmdheight=2
set showmatch
set nobackup
set ignorecase
set smartcase
set wildmode=list:longest,full
set showmode
set showcmd
set mouse=a
colorscheme elflord
syntax on



