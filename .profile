# For aurin, aursearch... aliases. (Also used in some other scripts)
export AUR_HELPER=trizen

# Changing this is not goint to change your keyboard layout. For more info, read ~/.scripts/switch-keyboard-layout
# See `localectl list-x11-keymap-layouts` for your options. (It's generally 2-char country code.)
# Set your default layout with `localectl set-keymap` and localectl set-x11-keymap
export KEYBOARD_LAYOUTS='us(intl)',tr

# To be able to deploy android apps from commandline.
export ANDROID_SDK_ROOT=/opt/android-sdk

# Apply GTK themes to QT apps (qt5-styleplugins)
export QT_QPA_PLATFORMTHEME=gtk2

# Ctrl-Shift-P to search menu items in GTK apps
if [[ -f /usr/lib/libplotinus.so ]]; then
    export GTK3_MODULES=/usr/lib/libplotinus.so
fi

# fzf stuff {{{
export FZF_BINDINGS="\
ctrl-d:page-down,ctrl-u:page-up,\
alt-j:preview-down,alt-k:preview-up,\
alt-d:preview-page-down,alt-u:preview-page-up,\
alt-e:execute($EDITOR {}),\
alt-y:execute-silent(echo {} | xcopy),\
ctrl-p:toggle-preview"

export FZF_DEFAULT_OPTS="
--bind \"$FZF_BINDINGS\" \
--header='(c-p: toggle-preview | c-d/u: page up/down | a-j/k: preview up/down | a-y: copy | a-e: edit)' \
--reverse"
# }}}

# Run ts_onfinish when a tsp job is finished
export TS_ONFINISH=ts_onfinish

# NPM make global installs in user directory
# nvm already uses user dir, so skip if nvm is found
if ! command -v nvm;; then
    export NPM_PACKAGES="$HOME/.npm-packages"
    export NODE_PATH="$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
fi

export GOPATH="$HOME/.go"
export R_LIBS_USER="$HOME/.rlibs"

# the PATH
export PATH=$HOME/.scripts:$HOME/.local/bin:$NPM_PACKAGES/bin:$GOPATH/.go/bin:$PATH

if command -v jaro; then
    export BROWSER=jaro
    export EDITOR="jaro --method=edit"
    export VISUAL=jaro
else
    export EDITOR=nvim
fi

export SHELL=/usr/bin/zsh

# Hyper + Y brings up a dmenu that lists the videos found in following playlist
export YT_MAIN_PLAYLIST=PLXawKvexOu0psiAqHCV5IFxdnWxZN1OVc

# Some constants
export PASSWORD_STORE=$HOME/Documents/sync/passwords.kdbx
export DIR_WALLPAPERS=$HOME/Pictures/wallpapers
export DIR_SCREENSHOTS=$HOME/Pictures/screenshots
export DIR_NOTES=$HOME/Documents/notes

if [[ -f /etc/profile.d/nix-daemon.sh ]]; then
    source /etc/profile.d/nix{,-daemon}.sh
fi


# Load profiles from /etc/profile.d
if test -d /etc/profile.d/; then
    for profile in /etc/profile.d/*.sh; do
        test -r "$profile" && . "$profile"
    done
    unset profile
fi

if [[ -f $HOME/.extrarc ]]; then
    . $HOME/.extrarc
fi

if ps 1 | grep sbin && file /sbin/init | grep systemd; then
    systemctl --user import-environment
    systemctl --user start user-login.target &
fi

# less colors (systemd can't import these) {{{
export LESS_TERMCAP_mb=$'\E[1;31m'     # begin bold
export LESS_TERMCAP_md=$'\E[1;36m'     # begin blink
export LESS_TERMCAP_me=$'\E[0m'        # reset bold/blink
export LESS_TERMCAP_so=$'\E[01;44;33m' # begin reverse video
export LESS_TERMCAP_se=$'\E[0m'        # reset reverse video
export LESS_TERMCAP_us=$'\E[1;32m'     # begin underline
export LESS_TERMCAP_ue=$'\E[0m'        # reset underline
export GROFF_NO_SGR=1                  # for konsole and gnome-terminal
# }}}

# vi: foldmethod=marker

