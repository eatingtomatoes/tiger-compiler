#!/usr/bin/env bash

set -e

function extract_title {
    sed -e "s/.*graph\(.*\){.*/\1/" | tr -d ' '
}

function extract_decs {
    tr '\n' ' ' | grep -P -o -e "(di)?graph.*?{.*?}"     
}

mkdir -p /tmp/dot
rm -f /tmp/dot/*

extract_decs | while read some; do
    TITLE=$(echo $some | extract_title)
    DOT_PATH=/tmp/dot/${TITLE}.dot
    SVG_PATH=/tmp/dot/${TITLE}.svg
    echo $some > $DOT_PATH
    dot -Tsvg $DOT_PATH -o $SVG_PATH
    display $SVG_PATH &
done

wait

