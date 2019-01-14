export BROWSER=firefox
export EDITOR=nvim
export VISUAL=nvim
export TERMINAL=st
export SHELL=/bin/fish
export PATH=$PATH:$HOME/.scripts:$HOME/.local/bin

# Used in calendar-popup script (I may forget to update this information)
export CALENDAR=ikhal

# For aurin, aursearch... aliases. (Also used in some other scripts)
export AUR_HELPER=trizen

# This script contains some utility functions, with exporting it like that
# I can easily use it in my scripts by adding: "source $UTILS_FILE"
export UTILS_FILE="$HOME/.scripts/utils"

# Changing this is not gonna change your keyboard layout. For more info, read ~/.scripts/switch-keyboard-layout
# See `localectl list-x11-keymap-layouts` for your options. (It's generally 2-char country code.)
# Set your default layout with `localectl set-keymap` and localectl set-x11-keymap
export KEYBOARD_LAYOUTS='us(intl)',tr

# To be able to deploy android apps from commandline.
export ANDROID_HOME=/opt/android-sdk

# Apply GTK themes to QT apps
export QT_QPA_PLATFORMTHEME=gtk2

# fff
export FFF_CD_FILE=~/.cache/fff/fff.d
export FFF_FAV1=/run/media/isa/
export FFF_FAV2=~/Workspace/projects/
export FFF_FAV3=~/Documents/
export FFF_FAV4=~/Videos/Shows/
export FFF_FAV5=~/Videos/Movies/

# Discard asserts and docstrings
export PYTHONOPTIMIZE=2

# Ctrl-Shift-P to search menu items in GTK apps
export GTK3_MODULES=/usr/lib/libplotinus.so

# Some fzf stuff
FZF_BINDINGS="\
ctrl-d:page-down,ctrl-u:page-up,\
alt-j:preview-down,alt-k:preview-up,\
alt-d:preview-page-down,alt-u:preview-page-up,\
alt-e:execute($EDITOR {}),\
alt-y:execute-silent(echo {} | xcopy),\
ctrl-p:toggle-preview"

export FZF_DEFAULT_OPTS="
--bind \"$FZF_BINDINGS\"
--preview 'bat --color=always --italic-text=always --number --line-range :200 {}'
--header='(c-p: toggle-preview | c-d/u: page up/down | a-j/k: preview up/down | a-y: copy | a-e: edit)'
--reverse
"
