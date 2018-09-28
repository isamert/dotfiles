# Defined in /tmp/fish.508A4S/c.fish @ line 2
function c
	switch $argv

    # ###########################################
    # CONFIG JUMPS
    # ###########################################

    case config
        cd ~/.config

    case profile
        eval $EDITOR ~/.profile


    case tmux
        eval $EDITOR ~/.tmux.conf


    case ranger
        cd ~/.config/ranger/


    case mutt
        cd ~/.config/mutt/
    case muttrc
        eval $EDITOR ~/.config/mutt/muttrc
    case gmailrc
        eval $EDITOR ~/.config/mutt/gmailrc


    case calcurse
        cd ~/.calcurse
    case calcurserc
        eval $EDITOR ~/.calcurse/conf


    case fish
        cd ~/.config/fish
    case funcs
        cd ~/.config/fish/functions


    case micro
        cd ~/.config/micro
    case microrc
        eval $EDITOR ~/.config/micro/settings.json

    case rofi
        cd ~/.config/rofi
    case rofirc
        eval $EDITOR ~/.config/rofi/config


    case bashrc
        eval $EDITOR ~/.bashrc


    case xbindkeys
        eval $EDITOR ~/.xbindkeysrc


    case my-config
        eval $EDITOR /etc/profile.d/my-config.sh


    case hosts
        eval $EDITOR /etc/hosts


    case mpd
        cd ~/.config/mpd
    case mpdrc
        eval $EDITOR ~/.config/mpd.conf


    case firefox
        cd ~/.mozilla/firefox/*.default
    case firefoxcsschrome
        eval $EDITOR ~/.mozilla/firefox/*.default/chrome/userChrome.css
    case firefoxcsscontent
        eval $EDITOR ~/.mozilla/firefox/*.default/chrome/userContent.css


    case newsboat
        cd ~/.newsboat
    case newsboatrc
        eval $EDITOR ~/.newsboat/config
    case newsboaturls
        eval $EDITOR ~/.newsboat/urls


    case vim
        cd ~/.config/nvim
    case vimrc
        eval $EDITOR ~/.config/nvim/init.vim


    case i3
        cd ~/.config/i3
    case i3rc
        eval $EDITOR ~/.config/i3/config
    case i3blocks
        eval $EDITOR ~/.config/i3/i3blocks.conf
    case compton
        eval $EDITOR ~/.config/compton.conf
    case polybar
        cd ~/.config/polybar
    case polybarrc
        eval $EDITOR ~/.config/polybar/config


    case systemd
        cd ~/.config/systemd/user/

    case qute
        cd ~/.config/qutebrowser
    case quterc
        eval $EDITOR ~/.config/qutebrowser/config.py
    case qutelocal
        cd ~/.local/share/qutebrowser

    case mpv
        cd ~/.config/mpd
    case mpvrc
        eval $EDITOR ~/.config/mpv/mpv.conf
    case mpvinputrc
        eval $EDITOR ~/.config/mpv/input.conf
    # ###########################################
    # PERSONAL JUMPS
    # ###########################################
    case notes
        cd ~/Documents/notes
    case work
        cd ~/Workspace
    case temp
        cd ~/Workspace/temp
    case projects
        cd ~/Workspace/projects

end
end
