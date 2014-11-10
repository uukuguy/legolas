directory ./edbroker:./server:./client:./base:./net:./deps/czmq/src

set args --trace  

handle SIGPIPE nostop print
b main
#b handle_pullin_on_local_frontend
run
