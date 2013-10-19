#!/bin/sh
for d in dev/dev*; do $d/bin/mynode $1; done
#for d in dev/dev{2,3}; do $d/bin/mynode-admin join mynode1@127.0.0.1; done
