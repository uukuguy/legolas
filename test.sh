#!/bin/bash
# ./test.sh num threads count
num=$1
if [ -z $num ]; then 
    num=1
fi

threads=$2
if [ -z $threads ]; then
    threads=0
fi

count=$3
if [ -z $count ]; then
    count=1
fi

server=$4
if [ -z $server ]; then
    server=127.0.0.1
fi

file=$5
if [ -z $file ]; then
    file=32K
fi

start=$(date "+%s")
for (( i = 0; i < $num ; i++ ))
do
    bin/legolas --write --threads $threads --count $count --server $server data/samples/$file.dat 
    echo ++++++++ $i ++++++++++
done

#sleep 2
now=$(date "+%s")
time=$((now - start))
echo "time used: $time secs."

