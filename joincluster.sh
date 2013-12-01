#!/bin/sh
for d in dev/dev{2,3,4}; do $d/bin/legolas-admin join legolas1@127.0.0.1; done
