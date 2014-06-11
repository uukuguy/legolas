
/etc/security/limits.conf

*    soft    noproc    65000
*    hard    noproc    65000
*    soft    nofile    65000
*    hard    nofile    65000

/etc/sysctl.conf

net.ipv4.ip_local_port_range = 1024 65000

check /proc/sys/net/ipv4/ip_local_port_range

/etc/fstab

/dev/sdb3 /mnt/data ext4 defaults,noatime 0 2

* network speed larger than disk wirte speed!! 


HyperDex

dependences:
libgoogle-glog-dev
libpopt-dev
libjson-c-dev
