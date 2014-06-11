#!/bin/sh
APP=legolas
#for d in dev/dev{2,3,4}; do $d/bin/${APP}-admin cluster join ${APP}1@127.0.0.1; done
dev/dev2/bin/${APP}-admin cluster join ${APP}1@127.0.0.1
dev/dev3/bin/${APP}-admin cluster join ${APP}1@127.0.0.1
dev/dev4/bin/${APP}-admin cluster join ${APP}1@127.0.0.1
#dev/dev5/bin/${APP}-admin cluster join ${APP}1@127.0.0.1
dev/dev1/bin/${APP}-admin member_status
dev/dev1/bin/${APP}-admin ring_status
