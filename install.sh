#!/bin/bash

# Exit on any error
set -e
set -o pipefail


# ############################################################################
# Utility functions
# ############################################################################

function is-arch {
    if [[ $(uname -r) = *arch ]]; then
        true
    else
        false
    fi
}

function install-package {
    # Check predefined package names
    # Detect current distribution
    # install with --noconfirm
    return
}

function gh-latest-release {
  curl --silent "https://api.github.com/repos/$1/releases/latest" | grep '"tag_name":' | sed -E 's/.*"([^"]+)".*/\1/'
}

# ############################################################################
# Specific installation recipes
# ############################################################################

function install-cmdutills {
    install-package git jq
}

function install-zsh {
    [[ -n $DISABLE_ZSH ]] && return

    echo "===== Installing zsh ====="

    curl -fLo ~/.local/bin/antigen.zsh --create-dirs https://git.io/antigen
    echo "Changing default shell to zsh..."
    chsh -s "$(command -v zsh)"
}

function install-elm {
    [[ -n $DISABLE_ELM ]] && return

    echo "===== Installing elm ====="

    local last_path=$(pwd)
    cd /tmp
    local elmver=$(gh-latest-release "elm/compiler")
    local binname=elm-${elmver}
    curl -L -o "${binname}.gz" "https://github.com/elm/compiler/releases/download/${elmver}/binary-for-linux-64-bit.gz"
    gunzip "${binname}.gz"
    chmod +x "${binname}"
    mkdir -p ~/.local/bin
    mv "${binname}" ~/.local/bin/elm
    cd "$last_path"
}

function install-jaro {
    [[ -n $DISABLE_JARO ]] && return

    echo "===== Installing jaro ====="

    local JARO_PATH=$HOME/.local/bin/jaro
    curl -fLo "$JARO_PATH" --create-dirs https://raw.githubusercontent.com/isamert/jaro/master/jaro
    chmod +x "$JARO_PATH"
    install-package guile
}

function install-nvim {
    [[ -n $DISABLE_NVIM ]] && return

    echo "===== Installing nvim ====="

    install-package neovim
    curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    echo "===>>> Run :PlugInstall after opening vim. <<<==="
}

function install-bspwm {
    ([[ -n $DISABLE_GUI ]] || [[ -n $DISABLE_BSPWM ]]) && return

    echo "===== Installing BSPWM ====="

    install-package bspwm sxhkd compton polybar bspwm-scripts
}

function install-mpv {
    ([[ -n $DISABLE_GUI ]] || [[ -n $DISABLE_MPV ]]) && return

    echo "===== Installing MPV ====="

    install-package mpv subdl

    # Install thumbnailer script
    mkdir -p ~/.config/mpv/scripts
    local thumbver=$(gh-latest-release "TheAMM/mpv_thumbnail_script")
    curl -fsSlL "https://github.com/TheAMM/mpv_thumbnail_script/releases/download/${thumbver}/mpv_thumbnail_script_client_osc.lua" > ~/.config/mpv/scripts/mpv_thumbnail_script_client_osc.lua
    curl -fsSlL "https://github.com/TheAMM/mpv_thumbnail_script/releases/download/${thumbver}/mpv_thumbnail_script_server.lua" > ~/.config/mpv/scripts/mpv_thumbnail_script_server.lua
}

function install-protonmail {
    ([[ -n $DISABLE_PROTON ]] || [[ -n $DISABLE_PROTONMAIL ]]) && return

    echo "===== Installing proton mail ====="

    install-package hydroxide-git
    # TODO: auth
}

function install-mpd {
    [[ -n $DISABLE_MPD ]] && return

    echo "===== Installing MPD ====="

    install-package mpd mpc ncmpcpp

    MPD_FOLDER=$HOME/.config/mpd
    mkdir -p "$MPD_FOLDER/playlists"
    touch "$MPD_FOLDER/mpd.db"
    touch "$MPD_FOLDER/mpd.log"
    touch "$MPD_FOLDER/mpdstate"

    systemctl --user enable mpd.socket
}

function install-aria2c {
    ([[ -n $DISABLE_ARIA2 ]] || [[ -n $DISABLE_ARIA2C ]]) && return

    echo "===== Installing aria2c ====="

    install-package aria2c

    ARIA2_CACHE_FOLDER=$HOME/.cache/aria2
    mkdir -p "$ARIA2_CACHE_FOLDER"
    touch "$ARIA2_CACHE_FOLDER/session"

    systemctl --user enable aria2c.service
}

function install-st {
    ([[ -n $DISABLE_GUI ]] || [[ -n $DISABLE_ST ]]) && return

    echo "===== Installing st ====="
    cd /tmp
    git clone https://github.com/isamert/st.git
    cd st
    sudo make install
    install-package ttf-symbola # To fix crashes on emojis
}

function install-r {
    [[ -n $DISABLE_ST ]] && return

    echo "===== Installing R ====="

    install-package r
    mkdir ~/.rlibs
}

function install-androiddev {
    [[ -n $DISABLE_ANDROID ]] && return

    echo "===== Configuring Android Dev ====="

    install-package android-tools android-udev

    if is-arch; then
        sudo usermod -aG adbusers "$LOGNAME"
    else
        sudo usermod -aG plugdev "$LOGNAME"
    fi
}

function install-firefox {
    ([[ -n $DISABLE_GUI ]] || [[ -n $DISABLE_FIREFOX ]]) && return

    echo "===== Installing firefox ====="

    install-package firefox
    curl -fsSl https://raw.githubusercontent.com/tridactyl/tridactyl/master/native/install.sh | bash
}

function install-mutt {
    ([[ -n $DISABLE_MUTT ]] || [[ -n $DISABLE_NEOMUTT ]]) && return

    echo "===== Installing neomutt ====="

    install-package neomutt

    mkdir -p ~/.cache/mutt
    for filename in ~/.config/mutt/accounts/*rc; do
        mkdir -p ~/.maildir/"$(basename "$filename" rc)"
        mkdir -p ~/.cache/mutt/"$(basename "$filename" rc)"
        mkdir -p ~/.cache/mutt/"$(basename "$filename" rc)"/bodies
    done
}

function install-nvidia {
    ([[ -n $DISABLE_GUI ]] || [[ -n $DISABLE_NVIDIA ]]) && return

    echo "===== Installing NVIDIA ====="

    install-package optimus-manager
}

function install-ranger {
    [[ -n $DISABLE_RANGER ]] && return

    echo "===== Installing Ranger ====="

    install-package ranger python-pip
    #pip install --user ueberzug # for image viewing
    mkdir -p ~/.config/ranger/plugins
    git clone https://github.com/alexanderjeurissen/ranger_devicons ~/.config/ranger/plugins/ranger_devicons # for icons
}

function install-trizen {
    [[ -n $DISABLE_TRIZEN ]] && return

    echo "===== Installing trizen ====="

    local last_path=$(pwd)
    cd /tmp
    git clone https://aur.archlinux.org/trizen.git
    cd trizen
    makepkg -si
    cd "$last_path"
}

# ############################################################################
# Specific configuration recipes
# ############################################################################

function configure-dotfiles {
    # This file is needed, computer specific Xres settings goes here
    touch ~/.Xresources.d/model

    configure-arch
    configure-ubuntu

    configure-systemd
    configure-systemduser
    configure-systemdroot

    configure-keyboard
}

function configure-arch {
    is-arch && return

    echo "===== Configuring ARCH ====="

    # Configure pacman
    sudo sed -i 's/^#Color$/Color/' /etc/pacman.conf
    sudo sed -i 's/^#VerbosePkgLists$/VerbosePkgLists/' /etc/pacman.conf

    # Install helpers
    install-trizen
}

function configure-ubuntu {
    # TODO: configure-ubuntu
    return
}

function configure-systemd {
    ([[ -n $DISABLE_SYSTEMD ]] || [[ -n $DISABLE_SYSTEMDCFG ]]) && return

    echo "===== Configuring systemd ====="

    sudo sed -i 's/^#DefaultTimeoutStopSec=\w*$/DefaultTimeoutStopSec=15s/' /etc/systemd/system.conf
}

function configure-systemduser {
    ([[ -n $DISABLE_SYSTEMDUSER ]] || [[ -n $DISABLE_SYSTEMDUSR ]]) && return

    echo "===== Configuring systemd user ====="

    if [[ -n $DISABLE_GUI ]]; then
        [[ -n $DISABLE_COPYQ ]] && systemctl --user enable --now copyq.service
        [[ -n $DISABLE_GEOCLUE ]] && systemctl --user enable --now geoclue-agent.service
        [[ -n $DISABLE_REDSHIFT ]] && systemctl --user enable --now redshift.service
        [[ -n $DISABLE_UNCLUTTER ]] && systemctl --user enable --now unclutter.service
        [[ -n $DISABLE_UDISKIE ]] && systemctl --user enable --now udiskie.service
        [[ -n $DISABLE_XCAPE ]] && systemctl --user enable --now xcape.service
        [[ -n $DISABLE_XCAPE || -n $DISABLE_XMODMAP ]] && systemctl --user enable --now monitor-kb-change.service
    fi

    [[ -z $DISABLE_SYNCTHING ]] && systemctl --user enable --now syncthing.service
}

# Some unit files needs to be run as root, I marked every unit file with #ROOT
# tag under ~/.config/systemd/user. This function copies them to root systemd
# unit directory and enables them.
function configure-systemdroot {
    [[ -n $DISABLE_SYSTEMDROOT ]] && return

    echo '===== Configuring systemd root ===='

    for file in "$HOME/.config/systemd/user/"*; do
        if [[ -f "$file" ]] && [[ $(head -n 1 "$file") =~ "#ROOT" ]]; then
            echo "Installing $file"
            sudo cp "$file" /usr/lib/systemd/system/
        fi
    done

    systemctl daemon-reload

    # Enable the unit files
    systemctl enable --now pacupd.timer
}

function configure-keyboard {
    [[ -n $DISABLE_KEYBOARD ]] && return

    # Change keyboard layout to us(intl)
    localectl set-x11-keymap 'us(intl)'
    # Make it take effect immediately:
    setxkbmap 'us(intl)'
}


for arg; do
    case "$arg" in
        --disable-*|--enable-*)
            # Convert --disable-xxx or --enable-xxx to DISABLE_XXX or ENABLE_XXX
            # and set it to 1
            TEMP_DISABLE="${arg/--/}"
            TEMP_DISABLE="${TEMP_DISABLE/-/_}"
            TEMP_DISABLE="${TEMP_DISABLE^^}"
            declare "$TEMP_DISABLE"=1
            ;;
    esac
done

if [[ $1 =~ install* ]] || [[ $1 =~ configure* ]]; then
    eval "${1}-${2}"
    exit 0
elif [[ $1 =~ --help ]]; then
    echo "=> Things you can disable"
    echo "=> Run the script with --disable-xxx option"
    grep -P --only-matching "DISABLE_\w+" ~/install.sh | sort -u
fi
