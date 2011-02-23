#!/usr/bin/env bash

xrandr --output HDMI-0 --mode 1920x1200 --output DVI-0 --primary --mode 1920x1200 --left-of HDMI-0

xsetroot -display $DISPLAY -cursor_name 'left_ptr' -solid black

xset -dpms
xset s off

xrdb -load $HOME/.Xresources
xmodmap $HOME/.Xmodmap

gnome-screensaver &

exec $HOME/.cabal/bin/xmonad
