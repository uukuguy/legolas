directory ./everdata:./server:./client:./base:./net:./deps/czmq/src

set args --trace --threads 1 --storage LMDB 

handle SIGPIPE nostop print
b main
run
