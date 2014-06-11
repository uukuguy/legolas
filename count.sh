#!/bin/bash

n="0"
k=$1
if [ -z $k ]; then
    for f in data/storage/*/*/*.dat ;
    do
        n=$[$n+1]
    done
else
    for f in $k ;
    do
        n=$[$n+1]
    done
fi
echo Total files: $n

