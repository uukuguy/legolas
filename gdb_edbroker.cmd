directory ./everdata:./server:./client:./net:./deps/ccl:./deps/czmq/src:./base

set args --trace  

handle SIGPIPE nostop print
b main
#b handle_pullin_on_local_frontend
run
