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
    Plug 'dkasak/gruvbox'

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

    " Search `easymotion` for keybindings
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

    " Distraction free writing
    Plug 'junegunn/goyo.vim'

    " Align stuff
    Plug 'godlygeek/tabular'

    Plug 'plasticboy/vim-markdown'

    Plug 'neovimhaskell/haskell-vim'
call plug#end()

" #############################################################################
"  ### GENERAL SETTINGS ###
" Helps Tab-completion in command mode for user commands (User commands must
" start with uppercase)
set smartcase
set mouse=a " It helps for resizing splits and stuff

" 4 spaced tabs
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
syntax on
filetype plugin indent on

" Spell
set spelllang=en_us,tr_tr

" Terminal inherits title
set title

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
" ### ORG MODE ###
let g:org_heading_shade_leading_stars = 0
let g:org_indent = 1

" ### MARKDOWN ###
let g:vim_markdown_toc_autofit = 1 " Shrink :Toc where possible
" #############################################################################


" #############################################################################
" ### LANGUAGE CLIENT SETTINGS ###
let g:LanguageClient_serverCommands            = {}
let g:LanguageClient_serverCommands["rust"]    = ['rustup', 'run', 'nightly', 'rls']
let g:LanguageClient_serverCommands["cpp"]     = ['clangd']
let g:LanguageClient_serverCommands["c"]       = ['clangd']
let g:LanguageClient_serverCommands["python"]  = ['pyls']
let g:LanguageClient_serverCommands["haskell"] = ['hie-wrapper']
" When gr(go references) is pressed, it will display a list that uses fzf with references
let g:LanguageClient_selectionUI = 'fzf'

map <leader>ll :call LanguageClient_contextMenu()<CR>
map <Leader>lh :call LanguageClient#textDocument_hover()<CR>
map <Leader>ld :call LanguageClient#textDocument_definition()<CR>
map <Leader>lr :call LanguageClient#textDocument_rename()<CR>
map <Leader>lf :call LanguageClient#textDocument_formatting()<CR>
map <Leader>lb :call LanguageClient#textDocument_references()<CR>
map <Leader>la :call LanguageClient#textDocument_codeAction()<CR>
map <Leader>ls :call LanguageClient#textDocument_documentSymbol()<CR>

" #### COMPLETION MANAGER ###
let g:deoplete#enable_at_startup = 1
let g:deoplete#auto_complete_delay = 150
let g:deoplete#max_list = 100
let g:deoplete#refresh_always = v:false
let g:deoplete#sources = {}
let g:deoplete#sources.c = ['LanguageClient']
let g:deoplete#sources.cpp = ['LanguageClient']
let g:deoplete#sources.python = ['LanguageClient']
let g:deoplete#sources.python3 = ['LanguageClient']
let g:deoplete#sources.rust = ['LanguageClient']
let g:deoplete#sources.haskell = ['LanguageClient']
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


" #############################################################################
" ### MY KEYBINDINGS ###
" Map leader key to space
nmap <space> <leader>

" Switch buffers with Alt+b and Alt+B
nnoremap <silent> <A-b> :bn<CR>
nnoremap <silent> <A-B> :bp<CR>

" Alt-. -> next tab
nnoremap <silent> <A-.> :tabnext<CR>
tnoremap <silent> <A-.> <C-\><C-n>:tabnext<CR>

" Alt-, -> prev tab
nnoremap <silent> <A-,> :tabprevious<CR>
tnoremap <silent> <A-,> <C-\><C-n>:tabprevious<CR>

" Alt-NUMBER goes to nth tab
nnoremap <silent> <A-1> :1 tabn<CR>
nnoremap <silent> <A-2> :2 tabn<CR>
nnoremap <silent> <A-3> :3 tabn<CR>
nnoremap <silent> <A-4> :4 tabn<CR>
nnoremap <silent> <A-5> :5 tabn<CR>

" Old habbits
nnoremap <silent> <C-t> :tabnew<CR>

" jj in edit mode emulates ESC (has timeout)
inoremap jj <ESC>
inoremap jk <ESC>

" easymotion | Jump to word
map  <leader>w <Plug>(easymotion-bd-w)
nmap <leader>w <Plug>(easymotion-overwin-w)prefix)
nmap s <Plug>(easymotion-overwin-f)

" easymotion | Line motions, J downwards, L upwards
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)

" easymotion | Search using easymotion
let g:EasyMotion_smartcase = 1
map  / <Plug>(easymotion-sn)
omap / <Plug>(easymotion-tn)
map  n <Plug>(easymotion-next)
map  N <Plug>(easymotion-prev)

" Move visual lines
onoremap <silent> j gj
onoremap <silent> k gk
nnoremap <silent> j gj
nnoremap <silent> k gk
vnoremap <silent> j gj
vnoremap <silent> k gk

" FZF
nnoremap <leader><space> :Commands<CR>
nnoremap <leader>g :GFiles<CR>
nnoremap <leader>b :Buffers<CR>


" #############################################################################
" ### TERMINAL MODE ###
" jj in terminal mode emulates ESC (has timeout)
tnoremap jj <C-\><C-n>
tnoremap jk <C-\><C-n>

" Exit terminal mode with ESC
tnoremap <Esc> <C-\><C-n>

" F9 -> new tab, open file dialog
nnoremap <F9> :tabe %:p:h<CR>

" Make Tab and S-Tab work visual mode
vnoremap <Tab> >gv
vnoremap <S-Tab> <gv
inoremap <S-Tab> <C-d>
" #############################################################################


" #############################################################################
" ### MY COMMANDS ###
command! W w
command! Wq wq
command! Q q

command! ConfigReload so $MYVIMRC
command! ConfigEdit e $MYVIMRC

command! SpellCheckEn setlocal spell! spelllang=en_us
" TODO: turkish spell check? Default vim spellcheck does not work even with
" generated wordlist. Check vimchant
" command! SpellCheckTr setlocal spell! spelllang=tr_tr
" #############################################################################
