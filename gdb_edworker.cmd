directory ./everdata:./server:./client:./base:./net:./deps/ccl:./deps/czmq/src

set args --trace --threads 4 --writers 1 --storage LMDB 

handle SIGPIPE nostop print
b main
run
