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
Plug 'scrooloose/nerdtree'                   " tree like file manager
Plug 'Xuyuanp/nerdtree-git-plugin'           " git flags for nerdtree
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

" editing
Plug 'easymotion/vim-easymotion'   " (s. easymotion)
Plug 'tpope/vim-surround'          " (y|c)(motion)(anything-to-surround)
Plug 'tpope/vim-repeat'            " repat more stuff with .
Plug 'godlygeek/tabular'           " :Tabularize /(thing to align)
Plug 'milkypostman/vim-togglelist' " \q -> Toggle quicfix, \l -> Toggle list

" lint, code comp. new languages etc.
Plug 'w0rp/ale'                    " lint, code completion, other lsp features
Plug 'neovimhaskell/haskell-vim'   " for better highlighting
Plug 'dag/vim-fish'                " syntaxh highlighting and stuff for fish
Plug 'kovetskiy/sxhkd-vim'
Plug 'gabrielelana/vim-markdown'
Plug 'leafgarland/typescript-vim'
Plug 'ianks/vim-tsx'

Plug 'glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }

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

" nerdtree {{{
" close vim if the nerdtree is the only window remaining
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
" Use <TAB> as enter in NerdTree
" autocmd FileType nerdtree nmap <buffer> <CR> <TAB>

map <A-f> :NERDTreeToggle<CR>
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

" fzf (https://github.com/junegunn/fzf/blob/master/README-VIM.md#fzf-inside-terminal-buffer) {{{
let $FZF_DEFAULT_OPTS = '--layout=reverse --margin=1,4'
let g:fzf_layout = { 'window': 'call OpenFloatingWin()' }

function! OpenFloatingWin()
    let height = &lines - 3
    let width = float2nr(&columns - (&columns * 2 / 10))
    let col = float2nr((&columns - width) / 2)

    "Set the position, size, etc. of the floating window.
    "The size configuration here may not be so flexible, and there's room for further improvement.
    let opts = {
                \ 'relative': 'editor',
                \ 'row': height * 0.3,
                \ 'col': col + 30,
                \ 'width': width * 2 / 3,
                \ 'height': height / 2
                \ }

    let buf = nvim_create_buf(v:false, v:true)
    let win = nvim_open_win(buf, v:true, opts)

    "Set Floating Window Highlighting
    call setwinvar(win, '&winhl', 'Normal:Pmenu')

    setlocal
                \ buftype=nofile
                \ nobuflisted
                \ bufhidden=hide
                \ nonumber
                \ norelativenumber
                \ signcolumn=no
endfunction

function! RipgrepFzf(query, fullscreen)
    " Rg with bat preview focused on selected line
    let command_fmt = 'rg --column --line-number --no-heading --color=always --smart-case %s || true'
    let initial_command = printf(command_fmt, shellescape(a:query))
    let spec = {'options': ['--layout=reverse', '--query', a:query, '--preview', 'line={}; file=${line%%:*}; linum=${${line#*:}%%:*}; range=$(($linum - $LINES / 2)); range_cmd=$([[ $range -gt -1 ]] && echo "--line-range"); range_cmd_arg=$([[ $range -gt -1 ]] && echo $range:); bat --color=always --style=header,numbers "$file" --highlight-line $linum $range_cmd $range_cmd_arg']}
    call fzf#vim#grep(initial_command, 1, spec, a:fullscreen)
endfunction

let $FZF_DEFAULT_OPTS = '--layout=reverse --info=hidden'

command! -bang -nargs=?  Files call fzf#vim#files(<q-args>, {'options': ['--info=inline', '--preview', 'bat --color=always --style=header,numbers {}'], 'window': ''}, <bang>0)
command! -bang -nargs=?  GFiles call fzf#vim#files(<q-args>, {'options': ['--layout=reverse', '--info=inline', '--preview', 'bat --color=always --style=header,numbers {}']}, <bang>0)
command! -nargs=* -bang Rg call RipgrepFzf(<q-args>, <bang>0)
command! -bang -nargs=? GLog call fzf#vim#grep('git log --graph --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr"', 1, {'options': ['--ansi', '--preview', 'echo {} | grep -o "[a-f0-9]\{7\}" | head -1 | xargs -I % sh -c "git show --color=always %"']}, <bang>0)

" [Commands] --expect expression for directly executing the command
let g:fzf_commands_expect = 'alt-enter,ctrl-x'
inoremap <expr> <c-x><c-k> fzf#vim#complete('cat /usr/share/dict/words')

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
" }}}

" vi: foldmethod=marker
