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

$ tail -f rel/legolas/log/legolas_debug.log

Upload
$ curl -i -T datafile --header "Content-Type: application/octet-stream" http://localhost:19090/b1/f2/d3

Download
$ curl http://localhost:19090/b1/f2/d3 > result.dat

Delete
$ curo -i -X DELETE http://localhost:19090/b1/f2/d3

