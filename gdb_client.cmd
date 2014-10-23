directory ./server:./client:./base:./net

set args --trace --write --threads 1 --start 0 --count 1000 --ip 127.0.0.1 data/samples/32K.dat 

handle SIGPIPE nostop print
b udb_handle_write_response
b main
run
