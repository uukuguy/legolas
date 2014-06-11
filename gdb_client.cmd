directory ./server:./client:./base:./legolas
set args --trace --threads 0 --count 2 --key key0 --server 192.168.0.226 data/32K.dat
b main
run
c
