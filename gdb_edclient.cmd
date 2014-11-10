directory ./edclient:./server:./client:./base:./net:./deps/czmq/src

set args --trace --threads 1 --count 1000 ./data/samples/32K.dat

handle SIGPIPE nostop print
b main
run
