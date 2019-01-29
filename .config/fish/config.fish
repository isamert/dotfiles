# opam configuration
source "$HOME/.opam/opam-init/init.fish" > /dev/null 2> /dev/null; or true

# #############################################################################
# install fisherman and plugins in fishfile if fisherman is not found
# #############################################################################
if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

# #############################################################################
# vi-mode
# #############################################################################
function fish_modified_vi_key_bindings
    fish_vi_key_bindings
    bind -M insert \e\r accept-autosuggestion execute  # alt-enter
    bind -M insert \el  accept-autosuggestion          # alt-l
    bind -M insert \ek  history-search-backward        # alt-k
    bind -M insert \ej  history-search-forward         # alt-j
    bind -M insert \f   forward-char
    bind -M insert \b   backward-char
    bind -M insert -m default jk backward-char force-repaint
end

set fish_key_bindings fish_modified_vi_key_bindings

# #############################################################################
# theme stuff (bobthefish settings)
# https://github.com/oh-my-fish/oh-my-fish/blob/master/docs/Themes.md#bobthefish
# #############################################################################
set theme_color_scheme gruvbox
set theme_title_display_process yes
set -U theme_date_format "+%H:%M:%S"

# #############################################################################
# Aliases (I really don't want to create seperate file for each of these)
# #############################################################################
# package management
alias aur="eval $AUR_HELPER"
alias aurin="eval $AUR_HELPER -S"
alias aurs="eval $AUR_HELPER -Ss"
alias aurupg="eval $AUR_HELPER -Syu"
alias pac="sudo pacman"
alias pacin="sudo pacman -S"
alias pacs="pacman -Ss"
alias pacin="sudo pacman -S"
alias pacins="sudo pacman -U"    # Install from file
alias pacupd="sudo pacman -Sy"
alias pacupg="sudo pacman -Syu"
alias pacfile="pacman -Fs"       # Find package that contains given file
alias pacre="sudo pacman -R"     # Leave dependencies and configurations
alias pacrem="sudo pacman -Rns"
alias pacpac="fuzzy packages"    # A fuzzy, interactive package finder
alias fpac="fuzzy packages"

# process management
alias nameof="ps -o comm= -p" # Get the name of given PID
alias fuckall="killall -s 9"
alias fkill="fuzzy kill"

# utility
alias mktar='tar -cvf'
alias mkbz2='tar -cvjf'
alias mkgz='tar -cvzf'
alias ...="cd ../.."                     # .. works out of the box.
alias ....="cd ../../.."
alias df="df -H"
alias du="ncdu"
alias lls="ls -ltrh"
alias grep="grep --color=auto"
alias xcopy="xclip -selection clipboard" # some_cmd | xcopy -> copies to cb
alias xpaste="xclip -o"                  # paste cb content

# abbrv
alias v="nvim"
alias how="howdoi --color --all"

# git
alias g="git"
alias gs="git status"
alias ga="fuzzy git add"
alias gl="fuzzy git log"
alias gf="fuzzy git files"
alias gc="git commit -m"
alias gpm="git push origin master"

alias cfg="git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
alias ca="fuzzy git add --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
alias cl="fuzzy git log --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
alias cf="fuzzy git files --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"

# useful
alias clearvimswap="rm $HOME/.local/share/nvim/swap/*"

# stuff
alias ipaddr="curl https://api.ipify.org; echo ''"
alias ipinfo="curl https://ipinfo.io; echo ''"
function weather; curl -s "wttr.in/$argv" | less -R; end
alias download-website="wget --recursive --page-requisites --html-extension
    --convert-links --no-parent --limit-rate=500K" # Download a website completely
alias git-todo="rg 'FIXME|TODO'" # Use grep instead of rg if you don't have it
alias git-todo-count="rg -c --color never 'FIXME|TODO' | cut -d: -f2 | paste -sd+ | bc"

function f; fff "$argv"; cd (cat ~/.cache/fff/fff.d); end
function mkcd; mkdir -p "$argv"; cd "$argv"; end          # Make and cd to the dir
function cpcd; cp $argv[1] $argv[2]; and cd $argv[2]; end # Copy and go to the directory
function mvcd; mv $argv[1] $argv[2]; and cd $argv[2]; end # Move and cd to the dir
function cheat; curl http://cheat.sh/"$argv"; end
function url; curl -F"shorten=$argv" "https://0x0.st"; end
function upload; curl -F"file=@$argv" "https://0x0.st"; end
function p # commandline pastebin
    set var "$argv"
    [ -n "$var" ]; or read var
    set url (echo "$var" | curl -F 'f:1=<-' ix.io)
    echo "$url"; echo "$url" | xcopy
end