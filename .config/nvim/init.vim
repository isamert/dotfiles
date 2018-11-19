" ##################################################
"                   (_)
"	     __   ___ _ __ ___  _ __ ___
"	     \ \ / / | '_ ` _ \| '__/ __|
"	      \ V /| | | | | | | | | (__
"	     (_)_/ |_|_| |_| |_|_|  \___|
" ##################################################

" linters:
" aurin shellcheck-static -> bash linter

call plug#begin('~/.local/share/nvim/plugged')
" (s. something) means search something in this file to see relevant settings/keybindings

" aesthetics
Plug 'rakr/vim-one'                " i'm using this as airline theme
Plug 'dkasak/gruvbox'              " general theme
Plug 'mhinz/vim-startify'          " a nice startup screen
Plug 'vim-airline/vim-airline'     " powerline stuff
Plug 'junegunn/goyo.vim'           " distraction free writing

" utility
Plug 'junegunn/fzf.vim'                  " Fuzzy finder (s. FZF)
Plug 'airblade/vim-gitgutter'            " Show git changes
Plug 'rhysd/devdocs.vim'                 " :DevDocs -> open stuff in DevDocs
Plug 'jeffkreeftmeijer/vim-numbertoggle' " Toggle between relative and normal lines when needed

" editing
Plug 'easymotion/vim-easymotion'   " (s. easymotion)
Plug 'tpope/vim-surround'          " (y|c)(motion)(anything-to-surround)
Plug 'godlygeek/tabular'           " :Tabularize /(thing to align)
Plug 'milkypostman/vim-togglelist' " \q -> Toggle quicfix, \l -> Toggle list

" lint, code comp. and stuff
Plug 'w0rp/ale'                    " lint, code completion, other lsp features
Plug 'neovimhaskell/haskell-vim'   " for better highlighting
Plug 'dag/vim-fish'                " syntaxh highlighting and stuff for fish

" markup
Plug 'jceb/vim-orgmode'
Plug 'plasticboy/vim-markdown'
call plug#end()

" theme
colorscheme gruvbox                  " ...
let g:one_allow_italics = 1          " Italic comments for one theme
let g:gruvbox_italic=1               " Italic comments for gruvbox
let g:gruvbox_contrast_dark = 'hard' " ...
syntax on                            " enable syntax highlighting

" tabs and spaces
set mouse=a               " enable mouse (helps precise resizing etc)
set tabstop=4             " tab-char width
set shiftwidth=4          " indent-level width
set softtabstop=4         " column count inserted by the tab key
set expandtab             " tabs -> spaces
set smartindent           " do it smart
filetype plugin indent on " determine indent by plugins

" visuals
set background=dark                " rearranges colors for dark background
set colorcolumn=80                 " 80-col line
set termguicolors                  " true color support
set number relativenumber          " line numbers relative to current line ()
set cursorline                     " highlight current line
hi Normal guibg=none ctermbg=none| " transparent background

" search/completion
set ignorecase " ignore case while searching
set smartcase  " abc -> Abc and abc, Abc -> only Abc (works in combination with ^^)

" utility
set showmatch             " visually indicate matching parens
set autoread              " update buffer if file is edited externally
set title                 " terminal inherits title
set clipboard=unnamedplus " use system clipboard
set inccommand=nosplit    " show effects of a command live
set spelllang=en_us       " default spelllang
set signcolumn=yes        " removes flickering caused by lang server
set undofile              " saves undo history to file (nvim's undodir default is OK)
set completeopt=menu,menuone,preview,noselect,noinsert

" netrw (file browser)
" :help netrw-quickmap
let g:netrw_banner = 0       " remove banner
let g:netrw_liststyle = 3    " tree style listing
let g:netrw_browse_split = 4 " ...
let g:netrw_altv = 1         " spawn it at left split
let g:netrw_usetab = 1       " use tab for expanding/shrinking folders
let g:netrw_winsize = 10     " occupies 10% of window

" trailing spaces
set listchars=tab:▸\ ,trail:·       " Show trailing spaces and tabs
set list                            " ^^ enable it
autocmd BufWritePre * :%s/\s\+$//e  " remove trailing spaces on save

" airline
let g:airline_powerline_fonts = 1                " use nice-looking fonts
let g:airline_theme='one'                        " this is better than gruvbox
let g:airline#extensions#tabline#enabled = 2     " show buffers as tabs
let g:airline#extensions#tabline#fnamemod = ':t' " show only filename for buffer tabs

" startify (the thing that pops up when vim is started)
let g:startify_session_dir = '~/.config/nvim/sessions'
let g:startify_bookmarks = ['~/Workspace/projects', '~/Documents/notes']
let g:startify_lists = [
    \ { 'type': 'files',     'header': ['MRU']            },
    \ { 'type': 'sessions',  'header': ['Sessions']       },
    \ { 'type': 'bookmarks', 'header': ['Bookmarks']      },
    \ { 'type': 'commands',  'header': ['Commands']       },
    \ ]

" org-mode
let g:org_heading_shade_leading_stars = 0 " don't shade the stars in headers

" markdown
let g:vim_markdown_toc_autofit = 1 " shrink :Toc to min possible

" autocomplete key mappings (tab, stab to select next, prev completion from list)
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<cr>"
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif " Close preview menu when completion is done

nnoremap <silent> <C-k> <Plug>(ale_previous_wrap)| " prev error
nnoremap <silent> <C-j> <Plug>(ale_next_wrap)|     " next error
nnoremap <silent> K :ALEDetail<CR>

" ale
" let g:ale_lint_on_text_changed = 'never' " only lints when file is saved
let g:airline#extensions#ale#enabled = 1       " ...
let g:ale_sign_error = '◉'                     " ...
let g:ale_sign_warning = '◉'                   " ...
let g:ale_completion_enabled = 1               " ...
let g:ale_haskell_hie_executable='hie-wrapper' " usage of this is encouraged instead of hie

" ale linter config
let g:ale_linters            = {}
let g:ale_linters['python']  = ['pyls', 'flake8', 'mypy', 'pylint']
let g:ale_linters['rust']    = ['rls']
let g:ale_linters['c']       = ['clangd', 'cquery']
let g:ale_linters['haskell'] = ['hie']

" leader
nmap <space> <leader>

" tabs and buffers
nnoremap <silent> <A-l> :bn<CR>|                    " alt-l  -> next buffer
nnoremap <silent> <A-h> :bp<CR>|                    " alt-h  -> prev buffer
nnoremap <silent> <A-.> :tabnext<CR>|               " alt-.  -> next tab
tnoremap <silent> <A-.> <C-\><C-n>:tabnext<CR>|     " alt-.  -> next tab (terminal mode)
nnoremap <silent> <A-,> :tabprevious<CR>|           " alt-,  -> prev tab
tnoremap <silent> <A-,> <C-\><C-n>:tabprevious<CR>| " alt-,  -> prev tab (terminal mode)
nnoremap <silent> <A-1> :1 tabn<CR>|                " alt-1  -> goes to tab 1
nnoremap <silent> <A-2> :2 tabn<CR>|                " ^^
nnoremap <silent> <A-3> :3 tabn<CR>|                " ^^
nnoremap <silent> <A-4> :4 tabn<CR>|                " ^^
nnoremap <silent> <A-5> :5 tabn<CR>|                " ^^
nnoremap <silent> <C-t> :tabnew<CR>|                " ctrl-t -> new tab

" fast esc
inoremap jk <ESC>|         " jk escapes to normal mode
tnoremap jk <C-\><C-n>|    " jk escapes to normal mode (in terminal mode)
tnoremap <Esc> <C-\><C-n>| " esc escapes to normal mode

" indention mappings
vnoremap <Tab> >gv|     " tab indents in visual mode
vnoremap <S-Tab> <gv|   " s-tab de-indents in visual mode
inoremap <S-Tab> <C-d>| " s-tab de-indents in insert mode

" easymotion
map  <leader>w <Plug>(easymotion-bd-w)|             " \w -> jump to word
nmap <leader>w <Plug>(easymotion-overwin-w)prefix)| " ^^
nmap s <Plug>(easymotion-overwin-f)|                " jump to character
map <Leader>j <Plug>(easymotion-j)|                 " jump to line (downwards)
map <Leader>k <Plug>(easymotion-k)|                 " jump to line (upwards)

" search using easymotion
let g:EasyMotion_smartcase = 1
map  / <Plug>(easymotion-sn)
omap / <Plug>(easymotion-tn)
map  n <Plug>(easymotion-next)
map  N <Plug>(easymotion-prev)

" move visual lines (j,k works in traditional way)
onoremap <silent> j gj
onoremap <silent> k gk
nnoremap <silent> j gj
nnoremap <silent> k gk
vnoremap <silent> j gj
vnoremap <silent> k gk

" fzf bindings
nnoremap <leader><space> :Commands<CR>| " \<space> -> lists all commands
nnoremap <leader>g :GFiles<CR>|         " \g       -> list all git files
nnoremap <leader>b :Buffers<CR>|        " \b       -> list buffers

" Master Wq bindings
command! Wq wq
command! W w
command! Q q
nnoremap <silent> <CR> :nohlsearch<CR><CR>| " Clear search highlighting

" meta
command! ConfigReload so $MYVIMRC " reload vim config
command! ConfigEdit e $MYVIMRC    " edit vim config
command! RestartLSP ALEDisable|ALEStopAllLSPs|ALEEnable

" utility
command! SpellCheckEn setlocal spell! spelllang=en_us
" TODO: turkish spell check? Default vim spellcheck does not work even with
" generated wordlist. Check vimchant
