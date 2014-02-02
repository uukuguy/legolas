legolas
=======

A high performance, high concurrency, scalable distributed file system especially for massive small files. 

$ rebar create template=riak_core_multinode appid=legolas nodeid=legolas force=1

$ make deps

$ make rel

$ make devrel

$ ./startcluster.sh
$ ./joincluster.sh # 构建集群。仅需make devclean & make devvrel后执行一次。

$ ./dev/dev1/bin/legolas attach

$ rel/legolas/bin/legolas console
$ curl -i -H "Accept: application/octet-stream" http://localhost:19090

$ tail -f rel/legolas/log/legolas_debug.log

Upload
$ curl -i -T datafile -H "Content-Type: application/octet-stream" http://localhost:19090/b1/f2/d3

Download
$ curl http://localhost:19090/b1/f2/d3 > result.dat

Delete
$ curo -i -X DELETE http://localhost:19090/b1/f2/d3

* deps.tar.gz
打包了riak_kv-1.4.7 eleveldb-1.4.7 cowboy-0.9.0
注意：riak_kv-1.4.7只能在erl/otp 15以下版本编译。

* 存储设计
legolas采用riak_kv存储框架，全新设计了echunk存储后端（riak_kv的自带存储后端包括bitcask和leveldb）。
echunk使用文件块（chunk）实现数据存储，每个chunk最大64M（由echunk_max_size参数决定）。小等于16M的文件顺序保存在chunk中，大于16M的文件单独原文件存储。

* 名字空间
- legolas采用 {Bucket, Path} 名字空间规范，依靠一致性哈希算法自行定位实际存储节点，不需要名字服务器。
- 存储节点缺省使用echunk存储引擎，每个chunk最大64M（由echunk_max_size参数决定）。
- Bucket可以单独定义存储根路径，从而实现分级存储。
- Path由用户自定义，可以采用类似文件全路径方式（如：/Documents/imgs/my.jpeg）。


