#!/usr/bin/env bash

xrandr --output HDMI-0 --primary --mode 1920x1200 --output DVI-0 --mode 1920x1200 --right-of HDMI-0

xsetroot -display $DISPLAY -cursor_name 'left_ptr' -solid black

xset -dpms
xset s off

xrdb -load $HOME/.Xresources

gnome-screensaver &

eval $(gnome-keyring-daemon --start)
export GNOME-KEYRING-SOCKET
export GNOME-KEYRING-PID

exec $HOME/.cabal/bin/xmonad
