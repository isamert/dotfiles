# #############################################################################
# install fisherman and plugins in fishfile if fisherman is not found
# #############################################################################
if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

# #############################################################################
# key-bindings
# #############################################################################
function my_fish_key_bindings
    fish_vi_key_bindings
    fzf_key_bindings
    bind -M insert -m default jk backward-char force-repaint
end

set fish_key_bindings my_fish_key_bindings

# #############################################################################
# theme stuff (bobthefish settings)
# https://github.com/oh-my-fish/oh-my-fish/blob/master/docs/Themes.md#bobthefish
# #############################################################################
set theme_color_scheme gruvbox
set theme_title_display_process yes
set -U theme_date_format "+%H:%M:%S"

function fish_update_abbrs
    sed "s/alias/abbr/;s/=/ /" ~/.config/aliases | source
end

if not set -q abbrs_initialized
    fish_update_abbrs
    set --universal abbrs_initialized SET
end

function weather; curl -s "wttr.in/$argv" | less -R; end
function mkcd; mkdir -p "$argv"; cd "$argv"; end          # Make and cd to the dir
function cpcd; cp $argv[1] $argv[2]; and cd $argv[2]; end # Copy and go to the directory
function mvcd; mv $argv[1] $argv[2]; and cd $argv[2]; end # Move and cd to the dir
function cheat; curl http://cheat.sh/"$argv"; end
function shorten; curl -F"shorten=$argv" "https://0x0.st"; end
function upload; curl -F"file=@$argv" "https://0x0.st"; end
function pastebin # commandline pastebin
    set var "$argv"
    [ -n "$var" ]; or read -z var
    set url (echo "$var" | curl -F 'f:1=<-' ix.io)
    echo "$url"; echo "$url" | xcopy
end

