#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source $UTILS_FILE

# #############################################################################
# General settings
# #############################################################################
# ignorespaces: lines begin with a space are not saved in the history list.
# ignoredups:   lines matching the previous history entry to not be saved.
# ignoreboth:   shorthand for ignorespace and ignoredups.
export HISTCONTROL=ignoreboth

# change directory without typing cd
shopt -s autocd
# fix line wrapping issues after resizing terminal window
shopt -s checkwinsize
# append to history instead of overwriting it
shopt -s histappend
# ** pattern will match all files, directories and subdirectories
shopt -s globstar
# save multiline command in the same history entry
shopt -s cmdhist

# #############################################################################
# Aliases
# #############################################################################

# Package management
alias aur="\$AUR_HELPER"
alias aurin="\$AUR_HELPER -S"
alias aurs="\$AUR_HELPER -Ss"
alias aurupg="\$AUR_HELPER -Syu"
alias pac="sudo pacman"
alias pacin="sudo pacman -S"
alias pacs="pacman -Ss"
alias pacin="sudo pacman -S"
alias pacins="sudo pacman -U" # Install from file
alias pacupd="sudo pacman -Sy"
alias pacupg="sudo pacman -Syu"
alias pacfile="pacman -Fs" # Find package that contains given file
alias pacre="sudo pacman -R" # Leave dependencies and configurations
alias pacrem="sudo pacman -Rns" # Leave dependencies and configurations

# Process management
alias nameof="ps -o comm= -p" # Get the name of give PID
alias fuckall="killall -s 9"

# Utility
alias mktar='tar -cvf'
alias mkbz2='tar -cvjf'
alias mkgz='tar -cvzf'

# .. works out of the box. (search for "shopt -s autocd")
alias ...="cd ../.."
alias ....="cd ../../.."
alias df="df -H"
alias du="ncdu"
alias ls="ls -hN --color=auto --group-directories-first"
alias lls="ls -ltrh"
alias grep="grep --color=auto"
alias xcopy="xclip -selection clipboard" # some_cmd | xcopy -> copies to cb
alias xpaste="xclip -o" # paste cb content

# Abbrv
alias v="nvim"
alias g="git"
alias gs="git status"
alias ga="git add"
alias gc="git commit -m"
alias gpm="git push origin master"
alias rxbk="killall xbindkeys; xbindkeys"

# Stuff
alias ipaddr="curl https://api.ipify.org; echo ''"
alias ipinfo="curl https://ipinfo.io; echo ''"
function weather { curl -s "wttr.in/$*" | less -R;}
# Download a website completely:
alias download-website="wget --recursive --page-requisites --html-extension --convert-links --no-parent --limit-rate=500K"
# Use grep instead of rg if you don't have it
alias git-todo="rg 'FIXME|TODO'"
alias git-todo-count="rg -c --color never 'FIXME|TODO' | cut -d: -f2 | paste -sd+ | bc"

# #############################################################################
# Utility functions (functions I only use interactively, others go $UTIL_FILE)
# #############################################################################
# Make and cd to the dir
function mkcd { mkdir -p "$1"; cd "$1"; }
# Copy and go to the directory
function cpcd { cp "$1" "$2" && cd "$2"; }
# Move and cd to the dir
function mvcd { mv "$1" "$2" && cd "$2"; }

function cheat { curl http://cheat.sh/"$1"; }
function shortenurl { curl -F"shorten=$1" "https://0x0.st"; }
function uploadfile { curl -F"file=@$1" "https://0x0.st"; }
source ~/.scripts/j

# #############################################################################
# PS/Prompt
# #############################################################################
function git-prompt-todo {
    local count=$(git-todo-count)
    [[ -z "$count" ]] || echo ", ${BOLD}$count${R}"
}

function git-prompt {
    # Show current branch name if the directory is a git folder.
    # Branch name will be shown in red if there are non-staged files,
    # yellow if there are uncommitted changes, green if everything is committed

    local branch=$(git symbolic-ref -q HEAD 2> /dev/null)
    branch=${branch##refs/heads/}
    local text=""
    # Check if there are non-staged files
    if [[ ! -z $(git diff --name-only 2> /dev/null) ]]; then
        text="${F_DRED}"
    # Check if there are staged files
    elif [[ ! -z $(git diff --cached --name-only 2> /dev/null) ]]; then
        text="${F_DYELLOW}"
    else
        text="${F_DGREEN}"
    fi

    [[ -z "$branch" ]] || echo " (${BOLD}$text$branch${R}${F_YELLOW}$(git-prompt-todo)${R})"
}

# plcs :: print last command status, prints {!} in red&bold if $? is not 0
alias plcs='[[  $? != 0 ]]  && echo "$BOLD$F_RED{!} $R"'
# pswd :: print short working dir (~/Documents/foo/bar -> ~/D/f/${BOLD}bar${R})
alias pswd='pwd | sed -e "s@$HOME@~@" | sed -r "s@([^/]+$)@$BOLD\1$R@" | sed -re "s@([^/])[^/]+/@\1/@g"'
# FIXME: fix non-visible character problem
export PS1='$(plcs)$(pswd)$(git-prompt) \[$BOLD\]~>\[$R\] '
