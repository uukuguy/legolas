directory ./server:./client:./base:./legolas
# set args --trace --write --threads 1 --count 1000 --server 127.0.0.1 data/samples/32K.dat 
set args --trace --execute --read --key /test/default/0000/00001000-32K.dat read.dat 
b main
run
