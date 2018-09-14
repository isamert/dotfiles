" ##################################################
"                   (_)
"	     __   ___ _ __ ___  _ __ ___
"	     \ \ / / | '_ ` _ \| '__/ __|
"	      \ V /| | | | | | | | | (__
"	     (_)_/ |_|_| |_| |_|_|  \___|
" ##################################################
call plug#begin('~/.local/share/nvim/plugged')

    " Theme
    Plug 'rakr/vim-one'
    Plug 'colepeters/spacemacs-theme.vim'
    Plug 'liuchengxu/space-vim-dark'
    Plug 'morhetz/gruvbox'

    "Startup screen
    Plug 'mhinz/vim-startify'

    " Powerline-like bars
    Plug 'vim-airline/vim-airline'

    " Insert mode=number, Normal mode=relativenumber
    Plug 'jeffkreeftmeijer/vim-numbertoggle'

    " cs<old-surrounding><new-surrounding> to change the surroundings
    " cs'<a> 'tpope/vim-surround' -> <a>tpope/vim-surround</a>
    " ds' to delete surroundings.
    Plug 'tpope/vim-surround'

    " Replace default filemanager with ranger
    Plug 'airodactyl/neovim-ranger'

    " Language server client
    Plug 'autozimu/LanguageClient-neovim', {
        \ 'branch': 'next',
        \ 'do': 'bash install.sh',
        \ }

    " Completion manager
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

    " fzf documentation
    Plug 'junegunn/fzf.vim'

    " \l -> Toggle list
    " \q -> Toggle quicfix
    " <C-w>p -> switch between windows
    Plug 'milkypostman/vim-togglelist'

    " \w -> go to word using hints
    Plug 'easymotion/vim-easymotion'

    " org-mode
    Plug 'jceb/vim-orgmode'

    " C-a, C-x to increment/decrement dates as expected
    Plug 'tpope/vim-speeddating'

    Plug 'rhysd/devdocs.vim'

    " Make extensions aviable trough .
    " See here for adding extension support: https://github.com/tpope/vim-repeat
    Plug 'tpope/vim-repeat'

    " Show git changes
    Plug 'airblade/vim-gitgutter'

    " Distract free writing
    Plug 'junegunn/goyo.vim'

call plug#end()

" #############################################################################
"  ### GENERAL SETTINGS ###
" Helps Tab-completion in command mode for user commands (User commands must
" start with uppercase)
set ignorecase
set mouse=a " It helps for resizing splits and stuff

" 4 spaced tabs
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
filetype plugin indent on

" Use system clipboard
set clipboard=unnamedplus

" Show effects of a command live
set inccommand=nosplit

" Show trailing spaces and tabs
set listchars=tab:▸\ ,trail:·
set list
" Remove trailing spaces on save
autocmd BufWritePre * :%s/\s\+$//e
" #############################################################################


" #############################################################################
" ### Startify settings ###
let g:startify_session_dir = '~/.config/nvim/sessions'
let g:startify_bookmarks = ['~/Workspace/projects', '~/Documents/notes']
let g:startify_lists = [
    \ { 'type': 'files',     'header': [   'MRU']            },
    \ { 'type': 'sessions',  'header': [   'Sessions']       },
    \ { 'type': 'bookmarks', 'header': [   'Bookmarks']      },
    \ { 'type': 'commands',  'header': [   'Commands']       },
    \ ]
" #############################################################################


" #############################################################################
" ### THEME/LOOK&FEEL SETTINGS ###
set background=dark
set termguicolors
set number relativenumber
set colorcolumn=80

colorscheme gruvbox " spacemacs-theme, one, spacemacs-theme, space-vim-dark, gruvbox
let g:one_allow_italics = 1 " Italic comments for one theme
let g:gruvbox_italic=1 " Italic comments for gruvbox
hi Comment cterm=italic " Italic comments for space-vim-dark theme

" Transparent background
hi Normal guibg=NONE ctermbg=NONE

" #### AIRLINE SETTINGS ###
let g:airline_powerline_fonts = 1
let g:airline_theme='one'
let g:airline#extensions#tabline#enabled = 2 " Show buffers as tabs
let g:airline#extensions#tabline#fnamemod = ':t' " Show only filename for buffer tabs
" #############################################################################


" #############################################################################
" ### LANGUAGE CLIENT SETTINGS ###
let g:LanguageClient_serverCommands         = {}
let g:LanguageClient_serverCommands["rust"] = ['rustup', 'run', 'nightly', 'rls']
let g:LanguageClient_serverCommands["cpp"] = ['clangd']
let g:LanguageClient_serverCommands["c"] = ['clangd']
let g:LanguageClient_serverCommands["python"] = ['pyls']
" When gr(go references) is pressed, it will display a list that uses fzf with references
let g:LanguageClient_selectionUI = 'fzf'


" #### COMPLETION MANAGER ###
let g:deoplete#enable_at_startup = 1
let g:deoplete#sources = {}
let g:deoplete#sources.c = ['LanguageClient']
let g:deoplete#sources.cpp = ['LanguageClient']
let g:deoplete#sources.python = ['LanguageClient']
let g:deoplete#sources.python3 = ['LanguageClient']
let g:deoplete#sources.rust = ['LanguageClient']
let g:deoplete#sources.vim = ['nvim']
" #############################################################################


" #############################################################################
" ### LANGUAGE CLIENT MAPPINGS ###
" gh -> Show type
nnoremap <silent> gh :call LanguageClient#textDocument_hover()<CR>
" gd -> Go to definition
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
" gr -> References
nnoremap <silent> gr :call LanguageClient_textDocument_references()<CR>
" gf -> Apply suggested action
" nnoremap <silent> gf :call LanguageClient#textDocument_codeAction()<CR>
" F2 -> Rename variable etc.
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
" :Fmt -> Format source code
command! Fmt call LanguageClient_textDocument_formatting()
" :FmtRange -> Format selected part of source code
command! FmtRange call LanguageClient#textDocument_rangeFormatting()


" ### COMPLETION MANAGER MAPPIGS ###
" <TAB> to select pop-up completion
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"


" ### MY KEYBINDINGS ###
" jj in edit mode emulates ESC (has timeout)
inoremap jj <ESC>

" Move visual lines
onoremap <silent> j gj
onoremap <silent> k gk
nnoremap <silent> j gj
nnoremap <silent> k gk
vnoremap <silent> j gj
vnoremap <silent> k gk

" Map leader key to space
nmap <space> <leader>

" FZF
nnoremap <leader><space> :Commands<CR>
nnoremap <leader>g :GFiles<CR>
nnoremap <leader>b :Buffers<CR>


" ### TERMINAL ###
" jj in terminal mode emulates ESC (has timeout)
tnoremap jj <C-\><C-n>
" Exit terminal mode with ESC
tnoremap <Esc> <C-\><C-n>

" Alt-. -> next tab
nnoremap <silent> <A-.> :tabnext<CR>
tnoremap <silent> <A-.> <C-\><C-n>:tabnext<CR>
" Alt-, -> prev tab
nnoremap <silent> <A-,> :tabprevious<CR>
tnoremap <silent> <A-,> <C-\><C-n>:tabprevious<CR>
" <TAB> -> cycle trough open windows
nnoremap <silent> <TAB> <C-w><C-w>
" F9 -> new tab, open file dialog
nnoremap <F9> :tabe %:p:h<CR>
" Make Tab and S-Tab work visual mode
vnoremap <Tab> >gv
vnoremap <S-Tab> <gv
inoremap <S-Tab> <C-d>
" #############################################################################


" #############################################################################
" ### MY COMMANDS ###
command! ConfigReload so $MYVIMRC
command! ConfigEdit e $MYVIMRC
command! W w
" # TODO: add urlview (), no need xst has alt+u binding
" #############################################################################
