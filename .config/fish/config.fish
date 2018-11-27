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
    bind -M insert \e\r accept-autosuggestion execute # alt-enter
    bind -M insert \el accept-autosuggestion          # alt-l
    bind -M insert \ek history-search-backward        # alt-k
    bind -M insert \ej history-search-forward         # alt-j
    bind -M insert -m default jk backward-char force-repaint
end

set fish_key_bindings fish_modified_vi_key_bindings

# #############################################################################
# theme stuff
# #############################################################################
set theme_color_scheme gruvbox       # FIXME: can I make it play nicely with .Xresources colors?
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
alias pacrem="sudo pacman -Rns"  # Leave dependencies and configurations

# process management
alias nameof="ps -o comm= -p" # Get the name of give PID
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
alias g="git"
alias gs="git status"
alias ga="fuzzy git add"
alias gl="fuzzy git log"
alias gc="git commit -m"
alias gpm="git push origin master"
alias rxbk="killall xbindkeys; xbindkeys"

# stuff
alias ipaddr="curl https://api.ipify.org; echo ''"
alias ipinfo="curl https://ipinfo.io; echo ''"
function weather; curl -s "wttr.in/$argv" | less -R; end
alias download-website="wget --recursive --page-requisites --html-extension
    --convert-links --no-parent --limit-rate=500K" # Download a website completely
alias git-todo="rg 'FIXME|TODO'" # Use grep instead of rg if you don't have it
alias git-todo-count="rg -c --color never 'FIXME|TODO' | cut -d: -f2 | paste -sd+ | bc"

function mkcd; mkdir -p "$argv"; cd "$argv"; end          # Make and cd to the dir
function cpcd; cp $argv[1] $argv[2]; and cd $argv[2]; end # Copy and go to the directory
function mvcd; mv $argv[1] $argv[2]; and cd $argv[2]; end # Move and cd to the dir
function cheat; curl http://cheat.sh/"$argv"; end
function shortenurl; curl -F"shorten=$argv" "https://0x0.st"; end
function uploadfile; curl -F"file=@$argv" "https://0x0.st"; end
