Egnix
=====

Nginx plugins especially for UDS.

F5 issue fix
-------------
Add uds_prefix into nginx.conf when use F5 proxy.

Submodules
----------

flvplay(https://github.com/uukuguy/flvplay) - Play .flv .mp4 video using UDS access interface specification.

Downloaad
---------

    http://67.20.96.216/eginx-1.2.4.tar.gz

Install
-------

    $ git clone https://github.com/uukuguy/eginx.git
    $ cd eginx
    $ git submodule init
    $ git submodule update
    $ cd modules
    $ cd flvplay
    $ git pull origin master
    $ git checkout master
    $ cd ../rtmp
    $ git pull origin master
    $ git checkout master
    $ cd ../../

Install on windows 
-------
    tools:cygwin, mingw32, vs2010
    libs:boost, json-c, curl
    ====cgywin====
    $ git clone https://github.com/uukuguy/eginx.git
    $ cd eginx
    $ git checkout master
    $ make -f Makefile.win
    ====mingw32====
    $ cd src/nginx_win-1.54
    $ sh build_win32.sh
    ====vs2010 Command prompt====
    $ nmake -f objs\Makefile.w32 
Run
---

    $ sudo eginx/bin/starteginx.sh
    $ tail -f eginx/logs/error.log

Stop
----

    $ sudo eginx/bin/stopeginx.sh


