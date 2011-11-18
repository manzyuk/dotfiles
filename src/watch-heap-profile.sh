#!/bin/sh

head -`fgrep -n END_SAMPLE $1.hp | tail -1 | cut -d : -f 1` $1.hp \
  | hp2ps -c > $1.ps
gv --orientation=seascape $1.ps &
gvpsnum=$!
while [ 1 ] ; do
  sleep 10
  head -`fgrep -n END_SAMPLE $1.hp | tail -1 | cut -d : -f 1` $1.hp \
    | hp2ps -c > $1.ps
  kill -HUP $gvpsnum
done
