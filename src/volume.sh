#!/usr/bin/env bash

OUTPUT=`amixer sget Master | tail -1`
VOLUME=`echo $OUTPUT | sed -r 's/.*\[([0-9]*)%\].*/\1/' | awk '{printf "%3s",$1}'`
STATUS=`echo $OUTPUT | sed -r 's/.*\[(on|off)\].*/\1/'`

if [[ "$STATUS" == "off" ]]
then
    COLOUR="OrangeRed"
else
    COLOUR="PaleGreen"
fi

echo "<fc=DimGrey>VOLUME:</fc> <fc=$COLOUR>$VOLUME%</fc>"
