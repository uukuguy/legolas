#!/bin/sh
APP=legolas
for d in dev/dev*; do $d/bin/${APP} start; done
dev/dev1/bin/${APP}-admin member_status
dev/dev1/bin/${APP}-admin ring_status
