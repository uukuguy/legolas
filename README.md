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

Upload (PUT)
$ curl -i -T datafile -H "Content-Type: application/octet-stream" http://localhost:19090/b1/f2/d3

Upload (POST)
$ curl -i -F upload=@datafile -H "Content-Type: application/octet-stream" http://localhost:19090/b1/f2/d3

Download
$ curl http://localhost:19090/b1/f2/d3 > result.dat

Delete
$ curo -i -X DELETE http://localhost:19090/b1/f2/d3

* deps.tar.gz
打包了riak_kv-1.4.7 eleveldb-1.4.7 cowboy-0.9.0

* 存储设计
legolas采用riak_kv存储框架，全新设计了echunk存储后端（riak_kv的自带存储后端包括bitcask和leveldb）。
echunk使用文件块（chunk）实现数据存储，每个chunk最大64M（由echunk_max_size参数决定）。小等于16M的文件顺序保存在chunk中，大于16M的文件单独原文件存储。

* 名字空间
- legolas采用 {Bucket, Path} 名字空间规范，依靠一致性哈希算法自行定位实际存储节点，不需要名字服务器。
- 存储节点缺省使用echunk存储引擎，每个chunk最大64M（由echunk_max_size参数决定）。
- Bucket可以单独定义存储根路径，从而实现分级存储。不指定Bucket，缺省Bucket设为空串<<>>。
- Path由用户自定义，可以采用类似文件全路径方式（如：/Documents/imgs/my.jpeg）。
- Restful访问URL: /buckets/bucket/path/

针对上传文件返回fileid方式的兼容问题：fileid被全路。

/etc/security/limits.conf

*    soft    noproc    65000
*    hard    noproc    65000
*    soft    nofile    65000
*    hard    nofile    65000

/etc/sysctl.conf

net.ipv4.ip_local_port_range = 1024 65000

check /proc/sys/net/ipv4/ip_local_port_range

/etc/fstab

/dev/sdb3 /mnt/data ext4 defaults,noatime 0 2

* network speed larger than disk wirte speed!! 

dependences:
    jemalloc 
        Copyright (C) 2002-2014 Jason Evans <jasone@canonware.com>.
        All rights reserved.
        Copyright (C) 2007-2012 Mozilla Foundation.  All rights reserved.
        Copyright (C) 2009-2014 Facebook, Inc.  All rights reserved.
    libuv  
        Node's license
    leveldb 
        Copyright (c) 2011 The LevelDB Authors. All rights reserved.
    zeromq 
        GPL3
    czmq



HyperDex

dependences:
libgoogle-glog-dev
libpopt-dev
libjson-c-dev

libgflags-dev
libssl-dev
uuid-dev
libbz2-dev

g++ cscope exuberant-ctags git subversion automake cmake gnome-do vim-gtk cdargs
fcitx fcitx-table-wbpy fcitx-tools fcitx-m17n fcitx-config-gtk2 fcitx-googlepinyin fcitx-ui-classic fcitx-module-dbus fcitx-module-cloudpinyin  fcitx-table-wubi
libboost1.55-all-dev liblog4cxx10-dev
python-django python-sqlite
ipython ipython-notebook python-matplotlib ipython-qtconsole python-zmq  python-scipy
python-tornado 
libtool libjson0-dev libbz2-dev uuid-dev libsnappy-dev
from IPython.external.mathjax import install_mathjax
install_mathjax()

valgrind kcachegrind graphviz valkyrie alleyoop
eclipse-cdt-valgrind eclipse-cdt-valgrind-remote eclipse-cdt-profiling-framework

$ ipython notebook

google-chrome-stable_current_amd64.deb

* Start server
    ./bin/legolad
       or
    ./debug_server.sh

* Start client
    ./test.sh 1 1 0 10000 127.0.0.1 32K write
       or
    ./bin/legolas --write --threads 1 --start 0 --count 10000 --server 127.0.0.1 data/samples/32K.dat


How to use legolas client?


    udb_t *udb = udb_new("127.0.0.1", DEFAULT_PORT, (void*)user_data);
    int ret = udb_do(udb, on_ready);
    udb_free(udb);

    # ---------- write ----------
    udb->key, keylen, op_code, object_size 
    udb_write_data(udb, handle, buf, buf_size, after_write_object_slice, after_write_finished);

    int after_write_object_slice(udb_t *udb, msgidx_t *msgidx) {
        if ( !udb_is_write_done(udb) ){
            uint32_t total_writed = udb_get_writed_bytes(udb);

            UNUSED int ret;
            int handle = -1;
            ret = udb_append_data(udb, handle, buf, buf_size);
            return ret;
        }

        return 0;
    }

    int after_write_finished(udb_t *udb, message_t *response) {
        try_write_next_file(udb);

        return 0;
    }

    # ---------- read ----------
    udb->key, keylen, op_code 
    udb_read_data(udb, handle, after_read_object_slice, after_read_finished);
    

    int test_after_read_object_slice(udb_t *udb, msgidx_t *msgidx) {
        if ( msgidx->message->result != RESULT_SUCCESS ){
            try_read_next_file(udb);
        } else {
            msgidx->key,
            msgidx->data,
            msgidx->data_size,

            msgidx->object_size,
            msgidx->nslices,
            msgidx->slice_idx
        }

        return 0;
    }

    int after_read_finished(udb_t *udb, message_t *response) {
        try_read_next_file(udb);

        return 0;
    }

    # ---------- delete ----------
    udb_delete_data(udb, after_delete_finished);

    int after_delete_finished(udb_t *udb, message_t *response) {
        try_delete_next_file(udb);

        return 0;
    }


