#!/bin/sh
APP=legolas
for d in dev/dev*; do $d/bin/${APP} stop; done
LEGOLAS=`ps aux | grep legolas | grep -v grep | awk '{print $2}'`
kill -9 $LEGOLAS

