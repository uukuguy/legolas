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

start_index=$3
if [ -z $start_index ]; then
    start_index=0
fi

count=$4
if [ -z $count ]; then
    count=1
fi

server=$5
if [ -z $server ]; then
    server=127.0.0.1
fi

file=$6
if [ -z $file ]; then
    file=32K
fi

operation=$7
if [ -z $operation ]; then
    operation=write
fi

start=$(date "+%s")
for (( i = 0; i < $num ; i++ ))
do
    bin/legolas --$operation --threads $threads --start $start_index --count $count --server $server data/samples/$file.dat 
    echo ++++++++ $i ++++++++++
done

#sleep 2
now=$(date "+%s")
time=$((now - start))
echo "time used: $time secs."

