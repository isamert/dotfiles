export BROWSER=jaro
export EDITOR="jaro --method=edit"
export VISUAL=jaro
export SHELL=/bin/fish

# For aurin, aursearch... aliases. (Also used in some other scripts)
export AUR_HELPER=trizen

# Changing this is not goint to change your keyboard layout. For more info, read ~/.scripts/switch-keyboard-layout
# See `localectl list-x11-keymap-layouts` for your options. (It's generally 2-char country code.)
# Set your default layout with `localectl set-keymap` and localectl set-x11-keymap
export KEYBOARD_LAYOUTS='us(intl)',tr

# To be able to deploy android apps from commandline.
export ANDROID_HOME=/opt/android-sdk

# Apply GTK themes to QT apps
export QT_QPA_PLATFORMTHEME=gtk2

# fff {{{
export FFF_CD_FILE=~/.cache/fff/fff.d
export FFF_FAV1=/run/media/isa/
export FFF_FAV2=~/Workspace/projects/
export FFF_FAV3=~/Documents/
export FFF_FAV4=~/Videos/Shows/
export FFF_FAV5=~/Videos/Movies/
# }}}

# Ctrl-Shift-P to search menu items in GTK apps
export GTK3_MODULES=/usr/lib/libplotinus.so

# fzf stuff {{{
FZF_BINDINGS="\
ctrl-d:page-down,ctrl-u:page-up,\
alt-j:preview-down,alt-k:preview-up,\
alt-d:preview-page-down,alt-u:preview-page-up,\
alt-e:execute($EDITOR {}),\
alt-y:execute-silent(echo {} | xcopy),\
ctrl-p:toggle-preview"

export FZF_DEFAULT_OPTS="
--bind \"$FZF_BINDINGS\" \
--preview 'bat --color=always --italic-text=always --number --line-range :200 {}' \
--header='(c-p: toggle-preview | c-d/u: page up/down | a-j/k: preview up/down | a-y: copy | a-e: edit)' \
--reverse"
# }}}

# Run ts_onfinish when a tsp job is finished
export TS_ONFINISH=ts_onfinish

# SLIMMERJS requires firefox 59
export SLIMERJSLAUNCHER=$HOME/Workspace/temp/firefox/firefox-bin

# npm stuff (make it work for non-root global installs)
export NPM_PACKAGES="$HOME/.npm-packages"
export NODE_PATH="$NPM_PACKAGES/lib/node_modules:$NODE_PATH"

# the PATH
export PATH=$PATH:$HOME/.scripts:$HOME/.local/bin:$NPM_PACKAGES/bin

systemctl --user import-environment
systemctl --user start user-login.target &

# vi: foldmethod=marker
