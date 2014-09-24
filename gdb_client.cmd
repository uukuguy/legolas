directory ./server:./client:./base:./legolas
# set args --trace --execute --read --key /test/default/0000/00001000-32K.dat read.dat 
set args --trace --write --threads 10 --start 0 --count 1000 --server 127.0.0.1 data/samples/32K.dat 
# set args --trace --read --threads 2 --start 0 --count 5 --server 127.0.0.1 data/samples/32K.dat 
#set args --trace --delete --threads 10 --start 0 --count 500 --server 127.0.0.1 data/samples/32K.dat 
b main
run
