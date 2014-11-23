directory ./everdata:./server:./client:./base:./net:./deps/ccl:./deps/czmq/src

set args --trace --containers 1 --buckets 1 --storage LMDB 

handle SIGPIPE nostop print
b main
run
