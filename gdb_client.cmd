directory ./server:./client:./base:./legolas
set args --trace --write --threads 1 --count 5 --server 127.0.0.1 data/samples/32K.dat 
b main
run
