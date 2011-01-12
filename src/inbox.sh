#!/usr/bin/env bash

URL="https://mail.google.com/mail/feed/atom"
COUNT=`curl --netrc --silent "$URL" | tr -d '\n' | sed "s/.*<fullcount>\(.*\)<\/fullcount>.*/\1/"`

if [[ "$COUNT" == "0" ]]
then
    echo ""
else
    echo "<fc=DimGrey>INBOX:</fc> <fc=yellow>$COUNT</fc>"
fi