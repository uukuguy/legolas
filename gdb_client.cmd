directory ./server:./client:./base:./legolas
set args --trace --threads 4 --count 10 --server 192.168.0.226 data/32K.dat 32K
b main
run
c
