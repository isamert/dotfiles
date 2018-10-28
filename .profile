export BROWSER=firefox
export EDITOR=nvim
export VISUAL=nvim
export TERMINAL=st
export PATH=$PATH:$HOME/.scripts:$HOME/.local/bin

# Used in calendar-popup script (I may forget to update this information)
export CALENDAR=ikhal

# For aurin, aursearch... aliases.
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
