directory ./everdata:./server:./client:./base:./net:./deps/ccl:./deps/czmq/src

set args --trace --threads 1 --writers 4 --storage LMDB 

handle SIGPIPE nostop print
b main
run
