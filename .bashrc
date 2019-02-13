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

source ~/.config/aliases

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
