#!/bin/sh

xrdb -merge ~/.Xresources
xset s off                      # no screensaver
xset +dpms                      # turn on energy star
xset fp+ /usr/share/fonts/local # add local fonts
xsetroot -cursor_name left_ptr  # use a normal cursor
xsetroot -solid '#3f3f3f'       # set background color
xbindkeys                       # add keybindings, just monitor backlight for now

# for command in $(gnome-keyring-daemon --start); do
#     eval "export $command"
# done
# /usr/lib/gnome-settings-daemon/gnome-settings-daemon &
# nautilus --no-default-window --no-desktop &
# unclutter -idle 1 &
dunst &
dropboxd &
volti &
urxvtd -q -o -f
emacs --daemon

DEFAULT_SESSION=xmonad

case $1 in
awesome)
        exec awesome
        ;;
i3)
        exec i3
        ;;
xmonad)
        exec xmonad
        ;;
*)
        exec $DEFAULT_SESSION
        ;;
esac
