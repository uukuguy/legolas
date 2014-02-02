#!/bin/sh
APP=legolas
for d in dev/dev*; do $d/bin/${APP} stop; done
