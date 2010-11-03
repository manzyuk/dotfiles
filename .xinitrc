#!/usr/bin/env bash

xrandr --output HDMI-0 --mode 1920x1200 --output DVI-0 --mode 1920x1200 --right-of HDMI-0

xsetroot -display $DISPLAY -cursor_name 'left_ptr' -solid black

xmodmap $HOME/.Xmodmap

urxvtd -q -f -o

exec $HOME/.cabal/bin/xmonad
