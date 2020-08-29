[[ -z $__PROFILE_SOURCED ]] &&  . $HOME/.profile

# Plugins {{{
source ~/.local/bin/antigen.zsh

antigen use oh-my-zsh
antigen bundle git
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-history-substring-search
antigen bundle zsh-users/zsh-completions
antigen bundle kutsan/zsh-system-clipboard
antigen theme agnoster
antigen apply
# }}}

# history-substring-search settings {{{
# bind UP and DOWN arrow keys to history substring search
zmodload zsh/terminfo
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down
# }}}

# General settings {{{
setopt autocd histignoredups appendhistory incappendhistory histreduceblanks

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'       # Case insensitive tab completion
zstyle ':completion:*' rehash true                              # automatically find new executables in path
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000
HISTORY_SUBSTRING_SEARCH_FUZZY=1

bindkey -v                                                      # enable vi keybindings
# }}}

# Aliases {{{
source ~/.config/aliases
# }}}

# Source some files {{{
files=(
    /usr/share/fzf/key-bindings.zsh                             # fzf history search keybindings
    $HOME/.nix-profile/share/fzf/keybindings.zsh                # fzf history search keybindings
)

for file in $files; do
    [[ -f "$file" ]] && source $file
done
# }}}

# vterm integration (check out the file for more info) {{{
is-emacs && source ~/.config/zsh/emacs.sh
# }}}

# Override agnoster themes prompt_dir {{{
# https://github.com/sorin-ionescu/prezto/blob/master/modules/prompt/functions/prompt-pwd
function prompt_dir {
    setopt localoptions extendedglob

    # If we are in emacs vterm, send the current directory to emacs vterm
    # This establishes directory tracking
    is-emacs && vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";

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
