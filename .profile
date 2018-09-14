export BROWSER=firefox
export EDITOR=nvim
export VISUAL=nvim
export PATH=$PATH:$HOME/.scripts:$HOME/.local/bin

# To be able to deploy android apps from commandline.
export ANDROID_HOME=/opt/android-sdk

# For aurin, aursearch... functions in fish.
export AUR_HELPER=yaourt

# Changing this is not gonna change your keyboard layout. For more info, read ~/.scripts/switch-keyboard-layout
# See `localectl list-x11-keymap-layouts` for your options. (It's generally 2-char country code.)
export KEYBOARD_LAYOUTS='us(intl)',tr
