directory ./everdata:./server:./client:./base:./net:./deps/ccl:./deps/czmq/src

set args --trace --read --threads 1 --count 1000 ./data/samples/32K.dat

handle SIGPIPE nostop print
b main
run
