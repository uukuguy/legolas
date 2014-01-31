#!/bin/sh
for d in dev/dev*; do $d/bin/legolas start; done
dev/dev1/bin/legolas-admin member_status
dev/dev1/bin/legolas-admin ring_status
