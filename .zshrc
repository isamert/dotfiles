# Plugins {{{
source ~/.local/bin/antigen.zsh

antigen use oh-my-zsh
antigen bundle git
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-history-substring-search
antigen bundle isamert/zsh-abbrev-alias
antigen bundle zsh-users/zsh-completions
antigen theme agnoster
antigen apply
# }}}

# Settings {{{
setopt autocd histignoredups appendhistory incappendhistory histreduceblanks

HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000
HISTORY_SUBSTRING_SEARCH_FUZZY=1
# }}}

# Abbreviations {{{
abbrev-alias --init
source <(sed 's/alias/abbrev-alias -g/' ~/.config/aliases)
# }}}

# Key-bindings {{{
source /usr/share/fzf/key-bindings.zsh
# }}}

# Override agnoster themes prompt_dir {{{
# https://github.com/sorin-ionescu/prezto/blob/master/modules/prompt/functions/prompt-pwd
function prompt_dir {
    setopt localoptions extendedglob

    local current_pwd="${PWD/#$HOME/~}"
    local ret_directory

    if [[ "$current_pwd" == (#m)[/~] ]]; then
        ret_directory="$MATCH"
        unset MATCH
    elif zstyle -m ':prezto:module:prompt' pwd-length 'full'; then
        ret_directory=${PWD}
    elif zstyle -m ':prezto:module:prompt' pwd-length 'long'; then
        ret_directory=${current_pwd}
    else
        ret_directory="${${${${(@j:/:M)${(@s:/:)current_pwd}##.#?}:h}%/}//\%/%%}/${${current_pwd:t}//\%/%%}"
    fi

    unset current_pwd
    prompt_segment blue $CURRENT_FG "$ret_directory"
}
# }}}

# Utility functions (functions I only use interactively) {{{
function mkcd { mkdir -p "$1"; cd "$1"; } # Make and cd to the dir
function cpcd { cp "$1" "$2" && cd "$2"; } # Copy and go to the directory
function mvcd { mv "$1" "$2" && cd "$2"; } # Move and cd to the dir
function cheat { curl http://cheat.sh/"$1"; }
function shortenurl { curl -F"shorten=$1" "https://0x0.st"; }
function uploadfile { curl -F"file=@$1" "https://0x0.st"; }
# }}}

# vi: foldmethod=marker

