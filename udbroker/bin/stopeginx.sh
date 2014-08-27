#!/bin/sh

BINDIR=$(cd ${0%/*} && pwd)
ROOTDIR=${BINDIR%/*}
EGINX_HOME=${ROOTDIR}

start-stop-daemon --stop --pidfile ${EGINX_HOME}/logs/nginx.pid 
#--exec "${NGINX_HOME}/sbin/nginx -p ${NGINX_HOME}"
ps ax | grep -i nginx

