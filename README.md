legolas
=======

A high performance, high concurrency, scalable distributed file system especially for massive small files. 

$ rebar create template=riak_core_multinode appid=legolas nodeid=legolas force=1

$ make deps

$ make rel

$ make devrel

$ ./startcluster.sh

$ ./dev/dev1/bin/legolas attach

$ rel/legolas/bin/legolas console
$ curl -i -H "Accept: application/octet-stream" http://localhost:19090


