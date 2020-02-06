#!/usr/bin/env bash

function process {
    while read some; do
	echo $some
    done
}

tr '\n' ' ' < temp.txt | grep -P -o -e "digrap.*?{.*?}" | while read decs; do echo 
