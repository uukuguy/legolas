directory ./server:./client:./base:./legolas:./deps/libuv
set args --trace 
handle SIGPIPE nostop print
b main
run

