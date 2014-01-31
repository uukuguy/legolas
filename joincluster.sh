#!/bin/sh
#for d in dev/dev{2,3,4}; do $d/bin/legolas-admin join legolas1@127.0.0.1; done
dev/dev2/bin/legolas-admin join legolas1@127.0.0.1
dev/dev3/bin/legolas-admin join legolas1@127.0.0.1
dev/dev4/bin/legolas-admin join legolas1@127.0.0.1
dev/dev1/bin/legolas-admin member_status
dev/dev1/bin/legolas-admin ring_status
