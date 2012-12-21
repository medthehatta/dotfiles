set autoindent          " when making a new line, start at the current indent level
set autoread            " automatically reread a file if it was changed outside of vim
set cmdheight=2         " by making the output line taller, you don't have to 'Enter to Continue' as often
set expandtab           " turn tabs into spaces
set tabstop=2           " 2 spaces per tab
set shiftwidth=2        " 2 spaces per tab
set hidden              " allow changing buffers without saving
set hlsearch            " start with search highlighting on
set ignorecase          " do case-insensitive search, but see 'set smartcase'
set incsearch           " search incrementally
set mouse=a             " allow use of the mouse in cli mode
set nobackup            " don't save backup files
set number              " turn line numbering on
set ruler               " show the row and column in the statusbar
set showcmd             " like cmdheight, makes 'Enter to Continue' messages less frequent
set showmatch           " when a bracket is inserted, briefly flash the one it matches
set showmode            " in the status line, show the mode we're in
set smartcase           " when searching, only do case-sensitive searches if there's a capital letter
set wildmode=list:longest,full          " how to display completion lists
set gdefault            " when replacing, assume global replacement
set wrap                " wrap words...
set linebreak           " ...so that lines only break at 'breakable' characters


" toggle search highlighting
nnoremap <Space> :set hlsearch!<CR>

" autocomplete
inoremap ^<Space> ^n

" change command leader to semicolon
noremap ; :

" still use the semicolon functionality, but not the comma
noremap , ;

" open a line below
nnoremap K o<esc>

" in ReST, 'underline' the section
nnoremap gs yypVr

" comment line
nnoremap ,c :s/^/# /<CR>
vnoremap ,c :'<,'>s/^/# /<CR>

" uncomment line
nnoremap ,u :s/^# //<CR>
vnoremap ,u :'<,'>s/^# //<CR>

" quick way to leave insert mode (part one)
inoremap kj <esc>
inoremap jk <esc>

" quick write
nnoremap W :w<CR>

" i use C-r as my screen escape, so avoid it
inoremap <C-e> <C-r>                    

" For my rst->latex homework scripts
noremap <f5> :!./view<CR>               
noremap <f6> :!./upload<CR>
noremap <f7> :!./build<CR><CR>

colorscheme elflord     " Dark colorscheme

" Syntax highlighting and filetype-specific goodness
filetype plugin on      
syntax on       

" Automatically change into the file's local directory (but leave it on quit)
autocmd BufEnter * silent! lcd %:p:h "change into 

" Allow saving of files with sudo when I forgot to sudo vim
cnoremap w!! %!sudo tee > /dev/null %

" Sage notebooks are essentially python
autocmd BufRead,BufNewFile *.sage set filetype=python
" Arduino files are essentially C++
autocmd BufRead,BufNewFile *.ino set filetype=cpp

" Macros for homework editing
" copy last math
nnoremap gxm ?.. math<cr>Vj/\n\n<cr>y`^p
" make last equal sign an aligned equal
nnoremap gx= ?=<cr>a&<esc>`^


