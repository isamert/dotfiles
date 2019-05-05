" ##################################################
"                   (_)
"         __   ___ _ __ ___  _ __ ___
"         \ \ / / | '_ ` _ \| '__/ __|
"          \ V /| | | | | | | | | (__
"         (_)_/ |_|_| |_| |_|_|  \___|
" ##################################################

" linters:
" aurin shellcheck-static -> bash linter

" plugins {{{
call plug#begin('~/.local/share/nvim/plugged')
" aesthetics
Plug 'rakr/vim-one'                " i'm using this as airline theme
Plug 'dkasak/gruvbox'              " general theme
Plug 'mhinz/vim-startify'          " a nice startup screen
Plug 'vim-airline/vim-airline'     " powerline stuff
Plug 'junegunn/goyo.vim'           " distraction free writing

" utility
Plug 'terryma/vim-multiple-cursors'
Plug 'junegunn/fzf.vim'                      " Fuzzy finder (s. FZF)
Plug 'airblade/vim-gitgutter'                " Show git changes
Plug 'rhysd/devdocs.vim'                     " :DevDocs -> open stuff in DevDocs
Plug 'jeffkreeftmeijer/vim-numbertoggle'     " Toggle between relative and normal lines when needed
Plug 'majutsushi/tagbar'                     " list top-level stuff in a window
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

" editing
Plug 'easymotion/vim-easymotion'   " (s. easymotion)
Plug 'tpope/vim-surround'          " (y|c)(motion)(anything-to-surround)
Plug 'tpope/vim-repeat'            " repat more stuff with .
Plug 'godlygeek/tabular'           " :Tabularize /(thing to align)
Plug 'milkypostman/vim-togglelist' " \q -> Toggle quicfix, \l -> Toggle list

" lint, code comp. and stuff
Plug 'w0rp/ale'                    " lint, code completion, other lsp features
Plug 'neovimhaskell/haskell-vim'   " for better highlighting
Plug 'dag/vim-fish'                " syntaxh highlighting and stuff for fish

" markup
Plug 'gabrielelana/vim-markdown'
call plug#end()
" }}}

" theme {{{
colorscheme gruvbox                  " ...
let g:one_allow_italics = 1          " Italic comments for one theme
let g:gruvbox_italic=1               " Italic comments for gruvbox
let g:gruvbox_contrast_dark = 'hard' " ...
syntax on                            " enable syntax highlighting
" }}}

" visuals {{{
set background=dark                " rearranges colors for dark background
set colorcolumn=80                 " 80-col line
set termguicolors                  " true color support
set number relativenumber          " line numbers relative to current line ()
set cursorline                     " highlight current line
hi Normal guibg=none ctermbg=none| " transparent background
" }}}

" tabs and spaces {{{
set mouse=a               " enable mouse (helps precise resizing etc)
set tabstop=4             " tab-char width
set shiftwidth=4          " indent-level width
set softtabstop=4         " column count inserted by the tab key
set expandtab             " tabs -> spaces
set smartindent           " do it smart
filetype plugin indent on " determine indent by plugins
" }}}

" better defaults {{{
" search/completion
set ignorecase " ignore case while searching
set smartcase  " abc -> Abc and abc, Abc -> only Abc (works in combination with ^^)
set splitbelow
set splitright
set foldmethod=syntax " (indent, marker: fold between {{{ }}})
" }}}

" utility {{{
set showmatch             " visually indicate matching parens
set autoread              " update buffer if file is edited externally
set title                 " terminal inherits title
set clipboard=unnamedplus " use system clipboard
set inccommand=nosplit    " show effects of a command live
set spelllang=en_us       " default spelllang
set signcolumn=yes        " removes flickering caused by lang server
set undofile              " saves undo history to file (nvim's undodir default is OK)
set completeopt=menu,menuone,preview,noselect,noinsert
" }}}

" netrw (file browser) {{{
" :help netrw-quickmap
let g:netrw_banner = 0       " remove banner
let g:netrw_liststyle = 3    " tree style listing
let g:netrw_browse_split = 4 " ...
let g:netrw_altv = 1         " spawn it at left split
let g:netrw_usetab = 1       " use tab for expanding/shrinking folders
let g:netrw_winsize = 10     " occupies 10% of window
" }}}

" trailing spaces {{{
set listchars=tab:▸\ ,trail:·       " Show trailing spaces and tabs
set list                            " ^^ enable it
autocmd BufWritePre * :%s/\s\+$//e  " remove trailing spaces on save
" }}}

" airline {{{
let g:airline_powerline_fonts = 1                " use nice-looking fonts
let g:airline_theme='one'                        " this is better than gruvbox
let g:airline#extensions#tabline#enabled = 2     " show buffers as tabs
let g:airline#extensions#tabline#fnamemod = ':t' " show only filename for buffer tabs
" }}}

" startify (the thing that pops up when vim is started) {{{
let g:startify_session_dir = '~/.config/nvim/sessions'
let g:startify_bookmarks = ['~/Workspace/projects', '~/Documents/notes']
let g:startify_lists = [
    \ { 'type': 'files',     'header': ['MRU']            },
    \ { 'type': 'sessions',  'header': ['Sessions']       },
    \ { 'type': 'bookmarks', 'header': ['Bookmarks']      },
    \ { 'type': 'commands',  'header': ['Commands']       },
    \ ]
" }}}

" autocomplete key mappings (tab, stab to select next, prev completion from list) {{{
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<cr>"
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif " Close preview menu when completion is done
" }}}

" ale {{{
" let g:ale_lint_on_text_changed = 'never' " only lints when file is saved
let g:airline#extensions#ale#enabled = 1       " ...
let g:ale_sign_error = '◉'                     " ...
let g:ale_sign_warning = '◉'                   " ...
let g:ale_completion_enabled = 1               " ...
let g:ale_linters_explicit = 1                 " only run linters named in ale_linters settings.

" ale linter config (only use with linters, see below for lang servers)
let g:ale_linters             = {}
let g:ale_linters['sh']       = ['shellcheck']
let g:ale_linters['fish']     = ['fish']
let g:ale_linters['awk']      = ['gawk']
let g:ale_linters['r']        = ['lintr']
let g:ale_linters['vim']      = ['vint']
let g:ale_linters['json']     = ['jq']
let g:ale_linters['markdown'] = ['vale']
" }}}

" languageclient (for language servers) {{{
let g:deoplete#enable_at_startup = 1
let g:LanguageClient_serverCommands            = {}
let g:LanguageClient_serverCommands['haskell'] = ['hie-wrapper']
let g:LanguageClient_serverCommands['python']  = ['pyls']
let g:LanguageClient_serverCommands['rust']    = ['rustup', 'run', 'nightly', 'rls']
let g:LanguageClient_serverCommands['cpp']     = ['clangd']

" languageclient mappings
nnoremap <F5> :call LanguageClient_contextMenu()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
nnoremap <silent> K :call PreviewToggler('ViewDetails')<CR>
nnoremap <silent> E :call PreviewToggler('LanguageClient#explainErrorAtPoint')<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <leader>s :call LanguageClient#textDocument_documentSymbol()<CR>
" }}}

" stuff {{{
nmap <space> <leader>
inoremap jk <ESC>|         " jk escapes to normal mode
tnoremap jk <C-\><C-n>|    " jk escapes to normal mode (in terminal mode)
tnoremap <Esc> <C-\><C-n>| " esc escapes to normal mode
" }}}

" split mappings {{{
" next sections looks pretty much like my i3 config except Win key is replaced
" with the Alt key
" move between buffers with alt+hjkl
nnoremap <A-h> <C-w>h
nnoremap <A-j> <C-w>j
nnoremap <A-k> <C-w>k
nnoremap <A-l> <C-w>l

" faster resize for buffers
nnoremap <A-J> <C-w>+
nnoremap <A-K> <C-w>-
nnoremap <A-L> <C-w>>
nnoremap <A-H> <C-w><
tnoremap <A-J> <C-\><C-n><C-w>+
tnoremap <A-K> <C-\><C-n><C-w>-
tnoremap <A-L> <C-\><C-n><C-w>>
tnoremap <A-H> <C-\><C-n><C-w><

" faster split creation/deletion
nnoremap <silent> <A--> :split<CR>
nnoremap <silent> <A-\> :vsplit<CR>
nnoremap <silent> <A-w> :bd<CR>

" change buffers
nnoremap <silent> <C-l> :bn<CR>
nnoremap <silent> <C-h> :bp<CR>
" }}}

" tabs {{{
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
" }}}

" indention mappings {{{
vnoremap <Tab> >gv|     " tab indents in visual mode
vnoremap <S-Tab> <gv|   " s-tab de-indents in visual mode
inoremap <S-Tab> <C-d>| " s-tab de-indents in insert mode
" }}}

" easymotion {{{
map  <leader>w <Plug>(easymotion-bd-w)|             " \w -> jump to word
nmap <leader>w <Plug>(easymotion-overwin-w)prefix)| " ^^
nmap s <Plug>(easymotion-overwin-f)|                " jump to character
map <Leader>j <Plug>(easymotion-j)|                 " jump to line (downwards)
map <Leader>k <Plug>(easymotion-k)|                 " jump to line (upwards)

"search
let g:EasyMotion_smartcase = 1
map  / <Plug>(easymotion-sn)
omap / <Plug>(easymotion-tn)
map  n <Plug>(easymotion-next)
map  N <Plug>(easymotion-prev)
" }}}

" move visual lines (j,k works in traditional way) {{{
onoremap <silent> j gj
onoremap <silent> k gk
nnoremap <silent> j gj
nnoremap <silent> k gk
vnoremap <silent> j gj
vnoremap <silent> k gk
" }}}

" fzf bindings {{{
nnoremap <leader><space> :Commands<CR>| " \<space> -> lists all commands
nnoremap <leader>g :GFiles<CR>|         " \g       -> list all git files
nnoremap <leader>h :History<CR>|        " \h       -> list history
nnoremap <leader>b :Buffers<CR>|        " \b       -> list buffers
nnoremap <leader>f :Rg<CR>|             " \b       -> search in all lines of the project
" }}}

" Master Wq bindings {{{
command! Wq wq
command! W w
command! Q q
nnoremap <silent> <CR> :nohlsearch<CR><CR>| " enter -> clear search highlighting
nnoremap <silent> <C-s> :w<CR>|             " ctrl-s -> save
nnoremap <silent> <C-q> :q<CR>|             " ctrl-q -> quit
tnoremap <silent> <C-q> <C-\><C-n>:q<CR>|   " ctrl-q -> quit (term)
" }}}

" move in insert mode {{{
inoremap <C-l> <right>| " ctrl-l -> move right in insert mode
inoremap <C-h> <left>|  " ...
inoremap <C-j> <down>|  " ...
inoremap <C-k> <up>|    " ...
" }}}

" other {{{
vnoremap t :Tabularize/
nnoremap <leader>t :TagbarToggle<CR>
" }}}

" utility commands {{{
command! ConfigReload so $MYVIMRC " reload vim config
command! ConfigEdit e $MYVIMRC    " edit vim config
command! Vterm vsplit|term
command! Term split|term
command! SpellCheckEn setlocal spell! spelllang=en_us
command! RestartLSP call LanguageClient#exit() | call LanguageClient#startServer()
command! -range TabularizeHaskellData <line1>,<line2>GTabularize/[{},]\|::
" }}}

" autos {{{
autocmd BufWritePost ~/.Xresources,~/.Xdefaults !xrdb %
autocmd BufWritePost ~/.Xresources.d/* !xrdb ~/.Xresources
autocmd BufWritePost ~/.config/sxhkd/sxhkdrc !pkill -USR1 -x sxhkd
" }}}

" functions {{{
function! PreviewToggler(fn, ...)
    " Takes a function that opens previewwindow, if the pwindow is open then
    " closes it, if the pwindow is not open simply calls the function.
    for nr in range(1, winnr('$'))
        if getwinvar(nr, '&pvw') == 1
            pclose
            return 0
        endif
    endfor

    let params = get(a:, 1, [])
    :call call (function(a:fn), params)
endfunction

function! ViewDetails()
    if exists('b:LanguageClient_projectRoot')
        :call PreviewToggler('LanguageClient#textDocument_hover')
    else
        :ALEDetail
    endif
endfunction
" }}}

" vi: foldmethod=marker
