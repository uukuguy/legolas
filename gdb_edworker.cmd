directory ./edworker:./server:./client:./base:./net

set args --trace --threads 1 --storage_type LMDB 

handle SIGPIPE nostop print
b main
run
