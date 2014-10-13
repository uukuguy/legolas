SERVER = bin/legolasd
CLIENT = bin/legolas

BASE_OBJS = base/logger.o base/daemon.o base/coroutine.o base/react_utils.oo

BASE_OBJS += base/zmalloc.o base/work.o base/md5.o base/byteblock.o base/filesystem.o base/sysinfo.o
BASE_OBJS += base/skiplist.o base/adlist.o base/threadpool.o base/crc32.o base/http_parser.o

GENCCONT_OBJS = base/binary_tree.o base/chaining_hash_table.o base/dlist.o base/hash_shared.o base/linear_probing_hash_table.o base/range_binary_tree.o base/slist.o base/slist_queue.o

BASE_OBJS += ${GENCCONT_OBJS}

NET_OBJS = net/net.o net/message.o net/service.o net/session.o net/sockbuf_message.o

CC = gcc
LD = ld
AR = ar
#CC = nccgen -ncgcc -ncld -ncfabs
#LD = nccld
#AR = nccar

INSTRUMENT_OBJS = base/instrument.o base/function.o base/calltree.o
#GPROF_FLAGS = -pg
INSTRUMENT_FLAGS = -finstrument-functions
#CFLAGS_UCONTEXT=-D_XOPEN_SOURCE # ucontext.h error: The deprecated ucontext routines require _XOPEN_SOURCE to be defined.
COMMON_CFLAGS = ${GPROF_FLAGS} -g ${INSTRUMENT_FLAGS} -fPIC -m64 -Wall -D_GNU_SOURCE -I./include -I./net ${CFLAGS_UCONTEXT} 

KVDB_OBJS = base/kvdb.o 

KVDB_CFLAGS=-DHAS_LSM -I./deps/lsm
LSM_OBJS = base/kvdb_lsm.o

KVDB_OBJS += ${LSM_OBJS}

KVDB_CFLAGS += -DHAS_ROCKSDB
ROCKSDB_OBJS = base/kvdb_rocksdb.o 
KVDB_OBJS += ${ROCKSDB_OBJS}

KVDB_CFLAGS += -DHAS_LEVELDB
LEVELDB_OBJS = base/kvdb_leveldb.o 
KVDB_OBJS += ${LEVELDB_OBJS}

KVDB_CFLAGS += -DHAS_LMDB 
LMDB_OBJS = base/kvdb_lmdb.o 
KVDB_OBJS += ${LMDB_OBJS}

KVDB_CFLAGS += -DHAS_EBLOB
EBLOB_OBJS = base/kvdb_eblob.o
KVDB_OBJS += ${EBLOB_OBJS}

COMMON_CFLAGS += ${KVDB_CFLAGS}

SERVER_OBJS = server/main.o \
			  server/server.o \
			  server/session_handle.o \
			  server/session_handle_write.o \
			  server/session_handle_read.o \
			  server/session_handle_delete.o \
			  server/session_send.o \
			  server/operation.o \
			  server/storage.o \
			  server/vnode.o \
			  server/datazone.o \
			  server/logfile.o \
			  server/object.o

#SERVER_OBJS += ${INSTRUMENT_OBJS}

CLIENT_OBJS = client/main.o \
			  client/test.o \
			  client/client.o \
			  client/udb.o \
			  client/udb_write_data.o \
			  client/udb_read_data.o \
			  client/udb_delete_data.o \
			  client/client_write.o \
			  client/client_read.o \
			  client/client_delete.o \
			  client/client_session_handle.o \
			  client/client_execute.o

CLIENT_OBJS += ${INSTRUMENT_OBJS}


LIBUV=libuv-v0.11.22
JEMALLOC=jemalloc-3.6.0
LEVELDB=leveldb-1.15.0
LMDB=lmdb-0.9.14
ROCKSDB=rocksdb-3.2
LSM_SQLITE4=lsm-sqlite4
ZEROMQ=zeromq-4.0.4
CZMQ=czmq-2.2.0
ZYRE=zyre-1.0.0
MSGPACK=msgpack-c-cpp-0.5.9
CGREENLET=cgreenlet-20140730
CRUSH=crush-20140809
LTHREAD=lthread-20140730
PTH=pth-2.0.7
LIBCORO=libcoro-1.67

PCL=pcl-1.12
LIBLFDS=liblfds-6.1.1

REACT=react-2.3.1
EBLOB=eblob-0.22.8

UNAME := $(shell uname)

ifeq (${UNAME}, Linux)
	SO=so
endif

ifeq (${UNAME}, Darwin)
	SO=dylib
endif

.PHONY: udbroker server client deps data test

all: bin lib deps udbroker server client

udbroker:
	make -C udbroker

server: ${SERVER}

client: ${CLIENT}

# ---------------- deps ----------------
.PHONY: jemalloc libuv leveldb lmdb zeromq czmq zyre liblfds pcl react eblob

deps: jemalloc libuv leveldb lmdb lsm-sqlite4 zeromq czmq zyre msgpack cgreenlet crush lthread  pth libcoro rocksdb react eblob
#pcl 

# ................ jemalloc ................

CFLAGS_JEMALLOC=-DUSE_JEMALLOC -DHAVE_ATOMIC  -I./deps/jemalloc/include
#-DJEMALLOC_MANGLE
#-DJEMALLOC_NO_DEMANGLE 
#LDFLAGS_JEMALLOC=./deps/jemalloc/lib/libjemalloc.a
LDFLAGS_JEMALLOC=-L./deps/jemalloc/lib -ljemalloc
LDFLAGS_JEMALLOC=-ljemalloc

jemalloc: deps/jemalloc

deps/jemalloc:
	cd deps && \
	tar zxvf ${JEMALLOC}.tar.gz && \
	ln -sf ${JEMALLOC} jemalloc && \
	cd ${JEMALLOC} && \
	echo 3.6.0 > VERSION && \
	./autogen.sh && \
	./configure --with-jemalloc-prefix=je_ && \
	make build_lib && \
	cp -f lib/*.${SO} lib/*.a ../../lib/

# ................ libuv ................

CFLAGS_LIBUV=-I./deps/libuv/include
#LDFLAGS_LIBUV=./deps/libuv/.libs/libuv.a  
#LDFLAGS_LIBUV=-L./deps/libuv/.libs -luv -lpthread -lrt
LDFLAGS_LIBUV=-luv -lpthread 

libuv: deps/libuv

deps/libuv:
	cd deps && \
	tar zxvf ${LIBUV}.tar.gz && \
	ln -sf ${LIBUV} libuv && \
	cd ${LIBUV} && \
	./autogen.sh && \
	./configure && \
	make && \
	cp -f .libs/*.${SO} .libs/*.a ../../lib/

	#./gyp_uv.py -f make && \
	#BUILDTYPE=Release make -C out

# ................ leveldb ................

CFLAGS_LEVELDB=-I./deps/leveldb/include
#LDFLAGS_LEVELDB=./deps/leveldb/libleveldb.a
#LDFLAGS_LEVELDB=-L./deps/leveldb -lleveldb
LDFLAGS_LEVELDB=-lleveldb

leveldb: deps/leveldb

deps/leveldb:
	cd deps && \
	tar zxvf ${LEVELDB}.tar.gz && \
	ln -sf ${LEVELDB} leveldb && \
	cd ${LEVELDB} && \
	sed -i -e 's/()\s*;/(void);/g' include/leveldb/c.h && \
	make && \
	cp -f *.${SO} *.a ../../lib/

# ................ lmdb ................

CFLAGS_LMDB=-I./deps/lmdb
#LDFLAGS_LMDB=./deps/lmdb/liblmdb.a
#LDFLAGS_LMDB=-L./deps/lmdb -llmdb
LDFLAGS_LMDB=-llmdb

lmdb: deps/lmdb

deps/lmdb:
	cd deps && \
	tar zxvf ${LMDB}.tar.gz && \
	ln -sf ${LMDB} lmdb && \
	cd ${LMDB} && \
	make && \
	cp -f *.so *.a ../../lib/

	#cp -f *.${SO}* *.a ../../lib/

# ................ rocksdb ................

CFLAGS_ROCKSDB=-I./deps/rocksdb/include
#LDFLAGS_ROCKSDB=./deps/rocksdb/librocksdb.a -lbz2
#LDFLAGS_ROCKSDB=-L./deps/rocksdb -lrocksdb -lpthread -lstdc++ -lbz2
LDFLAGS_ROCKSDB=-lrocksdb -lpthread -lstdc++ -lbz2

rocksdb: deps/rocksdb

deps/rocksdb:
	cd deps && \
	tar zxvf ${ROCKSDB}.tar.gz && \
	ln -sf ${ROCKSDB} rocksdb && \
	cd ${ROCKSDB} && \
	sed -i -e 's/()\s*;/(void);/g' include/rocksdb/c.h && \
	sed -i -e 's/rocksdb::GetDeletedKey/(size_t)rocksdb::GetDeletedKey/' tools/sst_dump.cc && \
	MAKECMDGOALS=static_lib JEMALLOC_INCLUDE="-I../jemalloc/include" JEMALLOC_LIB="-L../jemalloc/lib -ljemalloc" make static_lib && \
	MAKECMDGOALS=shared_lib JEMALLOC_INCLUDE="-I../jemalloc/include" JEMALLOC_LIB="-L../jemalloc/lib -ljemalloc" make shared_lib && \
	cp -f *.${SO} *.a ../../lib/

	#MAKECMDGOALS=static_lib make 
	#sed -i -e 's/keys: %zd/keys: %zu/' tools/sst_dump.cc && \
	#sed -i -e 's/\$\(TESTS\)/\x23\$\(TESTS\)/' Makefile && \

# ................ lsm-sqlite4 ................

CFLAGS_LSM_SQLITE4=-I./deps/lsm
#LDFLAGS_LSM_SQLITE4=./deps/lsm/liblsm-sqlite4.a
#LDFLAGS_LSM_SQLITE4=-L./deps/lsm -llsm-sqlite4
LDFLAGS_LSM_SQLITE4=-llsm-sqlite4

lsm-sqlite4: deps/lsm

deps/lsm:
	cd deps && \
	tar zxvf ${LSM_SQLITE4}.tar.gz && \
	ln -sf ${LSM_SQLITE4} lsm && \
	cd ${LSM_SQLITE4} && \
	make && \
	cp -f *.so *.a ../../lib/

	#cp -f *.${SO}* *.a ../../lib/

# ................ zeromq ................

CFLAGS_ZEROMQ=-I./deps/zeromq/include
#LDFLAGS_ZEROMQ=./deps/zeromq/src/.libs/libzmq.a  
#LDFLAGS_ZEROMQ=-L./deps/zeromq/src/.libs -lzmq  
LDFLAGS_ZEROMQ=-lzmq  

zeromq: deps/zeromq

deps/zeromq:
	cd deps && \
	tar zxvf ${ZEROMQ}.tar.gz && \
	ln -sf ${ZEROMQ} zeromq && \
	cd ${ZEROMQ} && \
	./configure && \
	make && \
	ln -s src/.libs lib && \
	cp -f lib/*.${SO} lib/*.a ../../lib/

# ................ czmq ................

CFLAGS_CZMQ=-I./deps/czmq/include
#LDFLAGS_CZMQ=./deps/czmq/src/.libs/libczmq.a  
#LDFLAGS_CZMQ=-L./deps/czmq/src/.libs -lczmq  
LDFLAGS_CZMQ=-lczmq  

czmq: deps/czmq

deps/czmq:
	cd deps && \
	tar zxvf ${CZMQ}.tar.gz && \
	ln -sf ${CZMQ} czmq && \
	cd ${CZMQ} && \
	ZeroMQ_CFLAGS=-I`pwd`/../zeromq/include ZeroMQ_LIBS=-L`pwd`/../zeromq/src/.libs ./configure && \
	make && \
	ln -s src/.libs lib && \
	cp -f lib/*.${SO} lib/*.a ../../lib/

# ................ zyre ................

CFLAGS_ZYRE=-I./deps/zyre/include
#LDFLAGS_ZYRE=./deps/zyre/src/.libs/libzyre.a  
#LDFLAGS_ZYRE=-L./deps/zyre/src/.libs -lzyre  
LDFLAGS_ZYRE=-lzyre  

zyre: deps/zyre

deps/zyre:
	cd deps && \
	tar zxvf ${ZYRE}.tar.gz && \
	ln -sf ${ZYRE} zyre && \
	cd ${ZYRE} && \
	./autogen.sh && \
	./configure --with-libzmq=`pwd`/../zeromq --with-libczmq=`pwd`/../czmq  && \
	make && \
	ln -s src/.libs lib && \
	cp -f lib/*.${SO} lib/*.a ../../lib/

# ................ msgpack ................

CFLAGS_MSGPACK=-I./deps/msgpack/src
#LDFLAGS_MSGPACK=./deps/msgpack/src/.libs/libmsgpack.a  
#LDFLAGS_MSGPACK=-L./deps/msgpack/src/.libs -lmsgpack  
LDFLAGS_MSGPACK=-lmsgpack  

msgpack: deps/msgpack

deps/msgpack:
	cd deps && \
	tar zxvf ${MSGPACK}.tar.gz && \
	ln -sf ${MSGPACK} msgpack && \
	cd ${MSGPACK} && \
	./bootstrap && \
	./configure && \
	make && \
	ln -s src/.libs lib && \
	cp -f lib/*.${SO} lib/*.a ../../lib/

# ................ cgreenlet ................

CFLAGS_CGREENLET=-I./deps/cgreenlet/src
LDFLAGS_CGREENLET=-lgreenlet

cgreenlet: deps/cgreenlet

deps/cgreenlet:
	cd deps && \
	tar zxvf ${CGREENLET}.tar.gz && \
	ln -sf ${CGREENLET} cgreenlet && \
	cd ${CGREENLET} && \
	make && \
	cp -f src/*.a ../../lib/

# ................ crush ................

CFLAGS_CRUSH=-I./deps/crush
LDFLAGS_CRUSH=-lcrush

crush: deps/crush

deps/crush:
	cd deps && \
	tar zxvf ${CRUSH}.tar.gz && \
	ln -sf ${CRUSH} crush && \
	cd ${CRUSH} && \
	make && \
	cp -f *.${SO} *.a ../../lib/

# ................ lthread ................

CFLAGS_LTHREAD=-I./deps/lthread/src
LDFLAGS_LTHREAD=-llthread

lthread: deps/lthread

deps/lthread:
	cd deps && \
	tar zxvf ${LTHREAD}.tar.gz && \
	ln -sf ${LTHREAD} lthread && \
	cd ${LTHREAD} && \
	mkdir build && \
	cd build && \
	cmake .. && \
	make && \
	cp -f liblthread.a ../../../lib/

# ................ pth ................

CFLAGS_PTH=-I./deps/pth
LDFLAGS_PTH=-lpth

pth: deps/pth

deps/pth:
	cd deps && \
	tar zxvf ${PTH}.tar.gz && \
	ln -sf ${PTH} pth && \
	cd ${PTH} && \
	./configure --enable-optimize && \
	make && \
	cp -f .libs/*.${SO} .libs/libpth.a ../../lib/

# ................ libcoro ................

CFLAGS_LIBCORO=-I./deps/libcoro
LDFLAGS_LIBCORO=-lcoro

libcoro: deps/libcoro

deps/libcoro:
	cd deps && \
	tar zxvf ${LIBCORO}.tar.gz && \
	ln -sf ${LIBCORO} libcoro && \
	cd ${LIBCORO} && \
	make && \
	cp -f libcoro.a ../../lib/

# ................ liblfds ................

CFLAGS_LIBLFDS=-I./deps/liblfds/inc
#LDFLAGS_LIBLFDS=./deps/liblfds/bin/liblfds611.a 
LDFLAGS_LIBLFDS=-L./deps/liblfds/bin -llfds611 

liblfds: deps/liblfds

deps/liblfds:
	cd deps && \
	tar zxvf ${LIBLFDS}.tar.gz && \
	ln -sf ${LIBLFDS} liblfds && \
	cd ${LIBLFDS} && \
	make -f makefile.linux

# ................ pcl ................

CFLAGS_PCL=-I./deps/pcl/include
#LDFLAGS_PCL=./deps/pcl/pcl/.libs/libpcl.a  
LDFLAGS_PCL=-L./deps/pcl/pcl/.libs -lpcl  

pcl: deps/pcl

deps/pcl:
	cd deps && \
	tar zxvf ${PCL}.tar.gz && \
	ln -sf ${PCL} pcl && \
	cd ${PCL} && \
	./configure && \
	make 

# ................ react ................

CFLAGS_REACT=-I./deps/react/include
LDFLAGS_REACT=-lreact

react: deps/react

deps/react:
	cd deps && \
	tar zxvf ${REACT}.tar.gz && \
	ln -sf ${REACT} react && \
	cd ${REACT} && \
	patch -p1 < ../${REACT}.patch && \
	ln -s -r foreign/rapidjson include/react/ && \
	sed -i -e 's/react SHARED/react STATIC/g' CMakeLists.txt && \
	mkdir build && \
	cd build && \
	cmake -DCMAKE_BUILD_TYPE=Release .. && \
	make && \
	cp -f libreact.a ../../../lib/

# ................ eblob ................

CFLAGS_EBLOB=-I./deps/eblob/include
LDFLAGS_EBLOB=-leblob -leblob_react

eblob: deps/eblob

deps/eblob:
	cd deps && \
	tar zxvf ${EBLOB}.tar.gz && \
	ln -sf ${EBLOB} eblob && \
	cd ${EBLOB} && \
	patch -p1 < ../${EBLOB}.patch && \
	sed -i -e 's/SHARED/STATIC/g' library/CMakeLists.txt && \
	sed -i -e 's/SHARED/STATIC/g' bindings/cpp/CMakeLists.txt && \
	sed -i -e 's/SHARED/STATIC/g' bindings/python/CMakeLists.txt && \
	mkdir build && \
	cd build && \
	cmake -DREACT_INCLUDE_DIRS="`pwd`/../../react/include" -DREACT_LIBRARY_DIRS="`pwd`/../../react/build" -DCMAKE_BUILD_TYPE=Release .. && \
	make && \
	cp -f library/libeblob.a library/react/libeblob_react.a bindings/cpp/libeblob_cpp.a bindings/python/libeblob_python.a ../../../lib/

# ---------------- all ----------------
	
#CFLAGS_UCONTEXT=-D_XOPEN_SOURCE # ucontext.h error: The deprecated ucontext routines require _XOPEN_SOURCE to be defined.

COMMON_CFLAGS += ${CFLAGS_LIBUV} \
				 ${CFLAGS_JEMALLOC} \
				 ${CFLAGS_LEVELDB} \
				 ${CFLAGS_ROCKSDB} \
				 ${CFLAGS_LSM_SQLITE4} \
				 ${CFLAGS_LMDB} \
				 ${CFLAGS_ZYRE} ${CFLAGS_CZMQ} ${CFLAGS_ZEROMQ} \
				 ${CFLAGS_MSGPACK} \
				 ${CFLAGS_CGREENLET} \
				 ${CFLAGS_CRUSH} \
				 ${CFLAGS_LTHREAD} \
				 ${CFLAGS_PTH} \
				 ${CFLAGS_LIBCORO} \
				 ${CFLAGS_REACT} \
				 ${CFLAGS_EBLOB}
FINAL_CFLAGS = -std=c11 -Wstrict-prototypes \
			   ${COMMON_CFLAGS} \
			   ${CFLAGS}

#${CFLAGS_LIBLFDS} 
#${CFLAGS_PCL} 
#${CFLAGS_COLIB} 
#-DUSE_PRCTL
FINAL_CXXFLAGS=${COMMON_CFLAGS} -std=c++11 ${CXXFLAGS}

FINAL_LDFLAGS = ${GPROF_FLAGS} -L./lib 
ifeq (${UNAME}, Linux)
FINAL_LDFLAGS += -Wl,-rpath=../lib,-rpath=./lib
endif
FINAL_LDFLAGS += ${LDFLAGS_LIBUV} \
				${LDFLAGS_JEMALLOC} \
				${LDFLAGS_LEVELDB} \
				${LDFLAGS_ROCKSDB} \
				${LDFLAGS_LSM_SQLITE4} \
				${LDFLAGS_LMDB} \
				${LDFLAGS_ZYRE} \
				${LDFLAGS_CZMQ} \
				${LDFLAGS_ZEROMQ} \
				${LDFLAGS_MSGPACK} \
				${LDFLAGS_CGREENLET} \
				${LDFLAGS_CRUSH} \
				${LDFLAGS_LTHREAD} \
				${LDFLAGS_PTH} \
				${LDFLAGS_LIBCORO} \
				${LDFLAGS_EBLOB} \
				${LDFLAGS_REACT} \
				${LDFLAGS} -lsnappy -lpthread -lssl -lcrypto -lstdc++ -lm -lz

#${LDFLAGS_LIBLFDS} 
#${LDFLAGS_PCL} 
#${LDFLAGS_COLIB} 

ifeq (${UNAME}, Linux)
	FINAL_CFLAGS += -DOS_LINUX
	FINAL_LDFLAGS += /usr/lib/x86_64-linux-gnu/libuuid.a -lrt
	FINAL_LDFLAGS += -static
endif
ifeq (${UNAME}, Darwin)
	FINAL_CFLAGS += -DOS_DARWIN
	CFLAGS_UCONTEXT=-D_XOPEN_SOURCE # ucontext.h error: The deprecated ucontext routines require _XOPEN_SOURCE to be defined.
	FINAL_CFLAGS += ${CFLAGS_UCONTEXT}
endif

#${SERVER}: deps ${BASE_OBJS} ${SERVER_OBJS} ${NET_OBJS} bin
${SERVER}: ${BASE_OBJS} ${SERVER_OBJS} ${NET_OBJS} ${KVDB_OBJS}  
	${CC} -o ${SERVER} \
		${BASE_OBJS} \
		${SERVER_OBJS} \
		${NET_OBJS} \
		${KVDB_OBJS} \
		${FINAL_LDFLAGS} 


${CLIENT}: ${BASE_OBJS} ${CLIENT_OBJS} ${NET_OBJS} 
	${CC} -o ${CLIENT} \
		${BASE_OBJS} \
		${CLIENT_OBJS} \
		${NET_OBJS} \
		${FINAL_LDFLAGS}

bin:
	mkdir -p bin

lib:
	mkdir -p lib

%.oo: %.cpp
	${CC} ${FINAL_CXXFLAGS} -o $*.oo -c  $*.cpp

%.o: %.c
	${CC} ${FINAL_CFLAGS} -o $*.o -c  $*.c 

# ---------------- clean ----------------

clean:
	${MAKE} -C test clean && \
	rm -fr \
		${SERVER} \
		${CLIENT} \
		${BASE_OBJS} \
		${SERVER_OBJS} \
		${CLIENT_OBJS} \
		${NET_OBJS} \
		${LSM_OBJS} \
		${LEVELDB_OBJS} \
		${ROCKSDB_OBJS} \
		${KVDB_OBJS}

clean-deps:
	rm -fr \
		deps/${LIBUV} deps/libuv \
		deps/${JEMALLOC} deps/jemalloc \
		deps/${LEVELDB} deps/leveldb \
		deps/${LMDB} deps/rocksdb \
		deps/${ROCKSDB} deps/lmdb \
		deps/${LSM_SQLITE4} deps/lsm \
		deps/${ZEROMQ} deps/zeromq \
		deps/${CZMQ} deps/czmq \
		deps/${ZYRE} deps/zyre  \
		deps/${MSGPACK} deps/msgpack \
		deps/${CGREENLET} deps/cgreenlet \
		deps/${CRUSH} deps/crush \
		deps/${LTHREAD} deps/lthread \
		deps/${PTH} deps/pth \
		deps/${LIBCORO} deps/libcoro

cleanall: clean clean-deps
	rm -f lib/*

# ---------------- run ----------------

run:
	rm -fr data/storage && \
		rm -f memcheck.log && \
		./bin/legolasd

# ---------------- test ----------------

data:
	mkdir -p data/samples
	dd if=/dev/urandom of=data/samples/320.dat bs=320 count=1
	dd if=/dev/urandom of=data/samples/640.dat bs=640 count=1
	dd if=/dev/urandom of=data/samples/1K.dat bs=1K count=1
	dd if=/dev/urandom of=data/samples/32K.dat bs=1K count=32
	dd if=/dev/urandom of=data/samples/64K.dat bs=1K count=64
	dd if=/dev/urandom of=data/samples/100K.dat bs=1K count=100
	dd if=/dev/urandom of=data/samples/320K.dat bs=1K count=320
	dd if=/dev/urandom of=data/samples/640K.dat bs=1K count=640
	dd if=/dev/urandom of=data/samples/1M.dat bs=1M count=1
	dd if=/dev/urandom of=data/samples/2M.dat bs=1M count=2

test:
	${MAKE} -C test && \
		bin/test_legolas

check:
	@base64 result/result.dat > result/result.txt
	@base64 result/test.dat > result/test.txt
	@diff -s result/result.txt result/test.txt

check_result:
	@echo
	bin/legolas -k key0 result/test.dat
	@echo
	@ls -l result/*
	@echo
	#@${MAKE} --no-print-directory check
	@echo

rm_result:
	rm -f result/*

320: rm_result
	ln -s ../data/320.dat result/test.dat
	@${MAKE} --no-print-directory check_result

640: rm_result
	ln -s ../data/640.dat result/test.dat
	@${MAKE} --no-print-directory check_result

1K: rm_result
	ln -s ../data/1K.dat result/test.dat
	@${MAKE} --no-print-directory check_result

32K: rm_result
	ln -s ../data/32K.dat result/test.dat
	@${MAKE} --no-print-directory check_result

64K: rm_result
	ln -s ../data/64K.dat result/test.dat
	@${MAKE} --no-print-directory check_result

100K: rm_result
	ln -s ../data/100K.dat result/test.dat
	@${MAKE} --no-print-directory check_result

320K: rm_result
	ln -s ../data/320K.dat result/test.dat
	@${MAKE} --no-print-directory check_result

503K: rm_result
	ln -s ../data/503K.dat result/test.dat
	@${MAKE} --no-print-directory check_result

640K: rm_result
	ln -s ../data/640K.dat result/test.dat
	@${MAKE} --no-print-directory check_result

801K: rm_result
	ln -s ../data/801K.dat result/test.dat
	@${MAKE} --no-print-directory check_result

900K: rm_result
	ln -s ../data/900K.dat result/test.dat
	@${MAKE} --no-print-directory check_result

999K: rm_result
	ln -s ../data/999K.dat result/test.dat
	@${MAKE} --no-print-directory check_result

1M: rm_result
	ln -s ../data/1M.dat result/test.dat
	@${MAKE} --no-print-directory check_result

2M: rm_result
	ln -s ../data/2M.dat result/test.dat
	@${MAKE} --no-print-directory check_result

#VALGRIND_PROGRAME = bin/legolas 
#VALGRIND_PROGRAME_ARGS = --write --threads 2 --count 1000 --server 127.0.0,1 data/samples/32K.dat

VALGRIND_PROGRAME = bin/legolasd
VALGRIND_PROGRAME_ARGS =


#--num-callers=8 
#--max-stackframe=1064421600

memcheck:
	valgrind -v --tool=memcheck --suppressions=legolasd.supp --log-file=memcheck.log --leak-check=full --show-reachable=yes --track-origins=yes ${VALGRIND_PROGRAME} ${VALGRIND_PROGRAME_ARGS}

gen-supp:
	valgrind --tool=memcheck --gen-suppressions=all --log-file=supp.log ${VALGRIND_PROGRAME} ${VALGRIND_PROGRAME_ARGS}

vserver:
	valgrind --vgdb=yes --vgdb-error=0 --tool=memcheck --suppressions=legolasd.supp --leak-check=full --show-reachable=yes --track-origins=yes ${VALGRIND_PROGRAME} ${VALGRIND_PROGRAME_ARGS}

vgdb:
	gdb -x vgdb.cmd ${VALGRIND_PROGRAME}

#--leak-resolution=low  
#--num-callers=8
#--xml=yes --xml-file="memcheck.xml" 

callgrind:
	valgrind --tool=callgrind  --callgrind-out-file=callgrind.log ${VALGRIND_PROGRAME} ${VALGRIND_PROGRAME_ARGS}

#--separate-threads=yes

annotate:
	callgrind_annotate --inclusive=yes --tree=both --auto=yes callgrind.log > callgrind-out.log && \
	gprof2dot.py -f callgrind callgrind.log | dot -Tpng -o callgrind.png 

	#gprof ${VALGRIND_PROGRAME} ./gmon.out | gprof2dot.py | dot -Tpng -o gprof.png

#--separate-threads=yes

helgrind:

massif:

strace:
	strace -C -tt -T -f -o strace.log ${VALGRIND_PROGRAME} ${VALGRIND_PROGRAME_ARGS}

dd:
	dd if=/dev/zero of=dd.dat bs=1024k count=1024 conv=fdatasync

