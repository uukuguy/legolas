directory ./server:./client:./base:./legolas:./deps/libuv
set args --trace 
b main
echo "!!! target remote | vgdb --pid= ??\n"
target remote | vgdb
