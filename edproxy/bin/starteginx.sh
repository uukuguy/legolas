#!/bin/sh

BINDIR=$(cd ${0%/*} && pwd)
ROOTDIR=${BINDIR%/*}
EGINX_HOME=${ROOTDIR}

${EGINX_HOME}/sbin/nginx -p ${EGINX_HOME}
ps ax | grep -i nginx | grep -v grep

