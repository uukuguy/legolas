directory ./server:./client:./base:./net

set args --trace --write --threads 1 --start 0 --count 10000 --ip 127.0.0.1 --batch data/samples/32K.dat 

handle SIGPIPE nostop print
b main
b main_deconstructor
run
