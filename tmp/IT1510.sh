#! /bin/bash

echo "There are $(ls -l $1 | grep "^-" | wc -l) files in this directory"
echo "There are $(ls -l $1 | grep "^d" | wc -l) directories in this directory"
echo "$(ls -Sl $1 | sed 's/  */ /g' | cut -d " " -f9 | head -2 | tail -1) is the biggest file in this directory"
echo "$(ls -tl $1 | sed 's/  */ /g' | cut -d " " -f9 | head -2 | tail -1) is the most recently modified file in this directory"
echo "$(ls -l $1 | sed 's/  */ /g' | cut -d " " -f3 | tail -n +2 | sort | uniq) own files in this directory"

