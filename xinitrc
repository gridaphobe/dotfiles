#!/bin/bash

xrdb -merge ~/.Xresources

for command in $(gnome-keyring-daemon --start); do
    eval "export $command"
done
/usr/lib/gnome-settings-daemon/gnome-settings-daemon &
nautilus --no-default-window &
unclutter -idle 1 &
dunst &
dropboxd &
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
