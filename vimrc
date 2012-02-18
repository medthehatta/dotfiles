set autoindent          " when making a new line, start at the current indent level
set autoread            " automatically reread a file if it was changed outside of vim
set cmdheight=2         " by making the output line taller, you don't have to 'Enter to Continue' as often
set expandtab           " turn tabs into spaces
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


" toggle search highlighting
nnoremap <Space> :set hlsearch!<CR>

" change command leader to semicolon
noremap ; :

" still use the semicolon functionality, but not the comma
noremap , ;

" open a line below
nnoremap K o<esc>

" comment line
nnoremap ,c :'<,'>s/^/# /<CR>

" uncomment line
nnoremap ,u :s/^# //<CR>

" quick way to leave insert mode (part one)
inoremap kj <esc>
inoremap jk <esc>

" i use C-r as my screen escape, so avoid it
inoremap <C-e> <C-r>                    

" For my rst->latex homework scripts
noremap <f5> :!./view<CR>               
noremap <f6> :!./upload<CR>

" Easy switching between buffers
map <left> :bp!<CR> 
map <right> :bn!<CR> 

colorscheme elflord     " Dark colorscheme

" Syntax highlighting and filetype-specific goodness
filetype plugin on      
syntax on       

" Automatically change into the file's local directory (but leave it on quit)
autocmd BufEnter * silent! lcd %:p:h "change into 

" Sage notebooks are essentially python
autocmd BufRead,BufNewFile *.sage set filetype=python



