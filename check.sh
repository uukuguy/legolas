#!/bin/bash
k=$1
if [ -z $k ]; then
    k=32K
fi
o=data/samples/$k.dat
n="0"

for f in data/storage/*/*/*.dat ;
do
    n=$[$n+1]
    diff -q $f $o
done
echo Checking $o . Total files: $n

