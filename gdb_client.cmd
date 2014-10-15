directory ./server:./client:./base:./net

set args --trace --write --threads 1 --start 0 --count 10000 --server 127.0.0.1 data/samples/32K.dat 

handle SIGPIPE nostop print
b main
b main_deconstructor
run
