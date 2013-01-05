#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

xrdb -merge ~/.Xresources
xset +fp /usr/share/fonts/local # add local fonts
xset fp rehash
xsetroot -cursor_name left_ptr  # use a normal cursor
xsetroot -solid '#3f3f3f'       # set background color
xbindkeys                       # add keybindings, just monitor backlight for now
xcalib ~/.colorprofile          # use Apple's color profile for MBA screen

xfce4-power-manager --no-daemon &

keychain ~/.ssh/id_rsa
[ -f ~/.keychain/$HOSTNAME-sh ] && . ~/.keychain/$HOSTNAME-sh 2>/dev/null
[ -f ~/.keychain/$HOSTNAME-sh-gpg ] && . ~/.keychain/$HOSTNAME-sh-gpg 2>/dev/null

pcmanfm --daemon-mode --no-desktop &
feh --bg-fill ~/Pictures/snow-bg.jpg &
dropboxd &
nm-applet --sm-disable &
urxvtd -q -o -f &
emacs --daemon &

DEFAULT_SESSION=xmonad

case $1 in
awesome)
        exec awesome
        ;;
i3)
        exec i3
        ;;
kde)
        exec startkde
        ;;
stumpwm)
        exec stumpwm
        ;;
wmfs)
        exec wmfs
        ;;
xfce)
        exec startxfce4
        ;;
xmonad)
        exec xmonad
        ;;
*)
        exec $DEFAULT_SESSION
        ;;
esac
