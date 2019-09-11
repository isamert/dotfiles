" ##################################################
"                   (_)
"         __   ___ _ __ ___  _ __ ___
"         \ \ / / | '_ ` _ \| '__/ __|
"          \ V /| | | | | | | | | (__
"         (_)_/ |_|_| |_| |_|_|  \___|
" ##################################################


" visuals {{{
set background=dark                " rearranges colors for dark background
set colorcolumn=80                 " 80-col line
set termguicolors                  " true color support
set number relativenumber          " line numbers relative to current line ()
set cursorline                     " highlight current line
"hi Normal guibg=none ctermbg=none| " transparent background
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

" autocomplete key mappings (tab, stab to select next, prev completion from list) {{{
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<cr>"
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif " Close preview menu when completion is done
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

" move visual lines (j,k works in traditional way) {{{
onoremap <silent> j gj
onoremap <silent> k gk
nnoremap <silent> j gj
nnoremap <silent> k gk
vnoremap <silent> j gj
vnoremap <silent> k gk
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

" Turkish keyboard mappings {{{
nnoremap Ş :
nnoremap ı i
nnoremap ğ [
nnoremap ü ]
nnoremap Ğ {
nnoremap Ü }
nnoremap ç .
nnoremap Ö <
nnoremap Ç >
vnoremap Ş :
vnoremap ı i
vnoremap ğ [
vnoremap ü ]
vnoremap Ğ {
vnoremap Ü }
vnoremap ç .
vnoremap Ö <
vnoremap Ç >
" }}}

" move in insert mode {{{
inoremap <C-l> <right>| " ctrl-l -> move right in insert mode
inoremap <C-h> <left>|  " ...
inoremap <C-j> <down>|  " ...
inoremap <C-k> <up>|    " ...
" }}}

" utility commands {{{
command! ConfigReload so $MYVIMRC " reload vim config
command! ConfigEdit e $MYVIMRC    " edit vim config
command! Vterm vsplit|term
command! Term split|term
command! SpellCheckEn setlocal spell! spelllang=en_us
" }}}

" vi: foldmethod=marker
