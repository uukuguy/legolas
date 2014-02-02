#!/bin/sh
APP=legolas

#{"vm.swappiness",                        0, gte}, 
#{"net.core.wmem_default",          8388608, lte},
#{"net.core.rmem_default",          8388608, lte},
#{"net.core.wmem_max",              8388608, lte},
#{"net.core.rmem_max",              8388608, lte},
#{"net.core.netdev_max_backlog",      10000, lte},
#{"net.core.somaxconn",                4000, lte},
#{"net.ipv4.tcp_max_syn_backlog",     40000, lte},
#{"net.ipv4.tcp_fin_timeout",            15, gte},
#{"net.ipv4.tcp_tw_reuse",                1, eq}

ulimit -n 65536 # sysctl -n fs.file-max &  ulimit -n

for d in dev/dev*; do $d/bin/${APP} start; done

dev/dev1/bin/${APP}-admin member_status
dev/dev1/bin/${APP}-admin ring_status
