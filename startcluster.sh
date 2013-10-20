#!/bin/sh
for d in dev/dev*; do $d/bin/legolas start; done
for d in dev/dev{2,3}; do $d/bin/legolas-admin join legolas1@127.0.0.1; done
