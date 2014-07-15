SERVER = bin/legolasd
CLIENT = bin/legolas

LEGOLAS_OBJS = legolas/legolas.o legolas/protocol.o 

BASE_OBJS = base/logger.o base/daemon.o base/coroutine.o
BASE_OBJS += base/zmalloc.o base/work.o base/md5.o base/byteblock.o base/filesystem.o
BASE_OBJS += base/skiplist.o base/adlist.o base/crc32.o base/http_parser.o

#CFLAGS_UCONTEXT=-D_XOPEN_SOURCE # ucontext.h error: The deprecated ucontext routines require _XOPEN_SOURCE to be defined.
COMMON_CFLAGS = -ggdb -fPIC -m64 -Wall -D_GNU_SOURCE -I./include -I./legolas ${CFLAGS_UCONTEXT} 

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

COMMON_CFLAGS += ${KVDB_CFLAGS}

SERVER_OBJS = server/main.o \
			  server/server.o \
			  server/session.o \
			  server/session_handle_write.o \
			  server/session_handle_read.o \
			  server/session_handle_delete.o \
			  server/session_recv.o \
			  server/session_send.o \
			  server/operation.o \
			  server/storage.o \
			  server/vnode.o \
			  server/datazone.o \
			  server/logfile.o \
			  server/object.o

CLIENT_OBJS = client/main.o client/client.o client/client_write.o client/client_read.o client/client_delete.o


LIBUV=libuv-v0.11.22
JEMALLOC=jemalloc-3.6.0
LEVELDB=leveldb-1.15.0
LIBLMDB=liblmdb-0.9.13
ROCKSDB=rocksdb-3.2
LSM_SQLITE4=lsm-sqlite4
ZEROMQ=zeromq-4.0.4
CZMQ=czmq-2.2.0
ZYRE=zyre-1.0.0
MSGPACK=msgpack-c-cpp-0.5.9

PCL=pcl-1.12
COLIB=colib-20140530
LIBLFDS=liblfds-6.1.1

UNAME := $(shell uname)

.PHONY: server client deps data

all: deps bin server client

server: ${SERVER}

client: ${CLIENT}

# ---------------- deps ----------------
.PHONY: jemalloc libuv leveldb liblmdb zeromq czmq zyre liblfds pcl colib

deps: jemalloc libuv leveldb liblmdb rocksdb lsm-sqlite4 zeromq czmq zyre msgpack
#pcl 
#colib

# ................ jemalloc ................

CFLAGS_JEMALLOC=-DUSE_JEMALLOC -DHAVE_ATOMIC  -I./deps/jemalloc/include
#-DJEMALLOC_MANGLE
#-DJEMALLOC_NO_DEMANGLE 
LDFLAGS_JEMALLOC=./deps/jemalloc/lib/libjemalloc.a
#LDFLAGS_JEMALLOC=-L./deps/jemalloc/lib -ljemalloc

jemalloc: deps/jemalloc

deps/jemalloc:
	cd deps && \
	tar zxvf ${JEMALLOC}.tar.gz && \
	ln -sf ${JEMALLOC} jemalloc && \
	cd ${JEMALLOC} && \
	echo 3.6.0 > VERSION && \
	./autogen.sh && \
	./configure --with-jemalloc-prefix=je_ && \
	make build_lib

# ................ libuv ................

CFLAGS_LIBUV=-I./deps/libuv/include
#LDFLAGS_LIBUV=-L./deps/libuv/.libs -luv -lpthread -lrt
LDFLAGS_LIBUV=./deps/libuv/.libs/libuv.a  

libuv: deps/libuv

deps/libuv:
	cd deps && \
	tar zxvf ${LIBUV}.tar.gz && \
	ln -sf ${LIBUV} libuv && \
	cd ${LIBUV} && \
	./autogen.sh && \
	./configure && \
	make
	#./gyp_uv.py -f make && \
	#BUILDTYPE=Release make -C out

# ................ leveldb ................

CFLAGS_LEVELDB=-I./deps/leveldb/include
LDFLAGS_LEVELDB=./deps/leveldb/libleveldb.a

leveldb: deps/leveldb

deps/leveldb:
	cd deps && \
	tar zxvf ${LEVELDB}.tar.gz && \
	ln -sf ${LEVELDB} leveldb && \
	cd ${LEVELDB} && \
	sed -i -e 's/()\s*;/(void);/g' include/leveldb/c.h && \
	make 

# ................ liblmdb ................

CFLAGS_LIBLMDB=-I./deps/liblmdb
LDFLAGS_LIBLMDB=./deps/liblmdb/liblmdb.a

liblmdb: deps/liblmdb

deps/liblmdb:
	cd deps && \
	tar zxvf ${LIBLMDB}.tar.gz && \
	ln -sf ${LIBLMDB} liblmdb && \
	cd ${LIBLMDB} && \
	make 

# ................ rocksdb ................

CFLAGS_ROCKSDB=-I./deps/rocksdb/include
LDFLAGS_ROCKSDB=./deps/rocksdb/librocksdb.a -lbz2

rocksdb: deps/rocksdb

deps/rocksdb:
	cd deps && \
	tar zxvf ${ROCKSDB}.tar.gz && \
	ln -sf ${ROCKSDB} rocksdb && \
	cd ${ROCKSDB} && \
	sed -i -e 's/()\s*;/(void);/g' include/rocksdb/c.h && \
	MAKECMDGOALS=static_lib make 

# ................ lsm-sqlite4 ................

CFLAGS_LSM_SQLITE4=-I./deps/lsm
LDFLAGS_LSM_SQLITE4=./deps/lsm/liblsm-sqlite4.a

lsm-sqlite4: deps/lsm

deps/lsm:
	cd deps && \
	tar zxvf ${LSM_SQLITE4}.tar.gz && \
	ln -sf ${LSM_SQLITE4} lsm && \
	cd ${LSM_SQLITE4} && \
	make

# ................ zeromq ................

CFLAGS_ZEROMQ=-I./deps/zeromq/include
LDFLAGS_ZEROMQ=./deps/zeromq/src/.libs/libzmq.a  

zeromq: deps/zeromq

deps/zeromq:
	cd deps && \
	tar zxvf ${ZEROMQ}.tar.gz && \
	ln -sf ${ZEROMQ} zeromq && \
	cd ${ZEROMQ} && \
	./configure && \
	make && \
	ln -s src/.libs lib

# ................ czmq ................

CFLAGS_CZMQ=-I./deps/czmq/include
LDFLAGS_CZMQ=./deps/czmq/src/.libs/libczmq.a  

czmq: deps/czmq

deps/czmq:
	cd deps && \
	tar zxvf ${CZMQ}.tar.gz && \
	ln -sf ${CZMQ} czmq && \
	cd ${CZMQ} && \
	ZeroMQ_CFLAGS=-I`pwd`/../zeromq/include ZeroMQ_LIBS=-L`pwd`/../zeromq/src/.libs ./configure && \
	make && \
	ln -s src/.libs lib

# ................ zyre ................

CFLAGS_ZYRE=-I./deps/zyre/include
LDFLAGS_ZYRE=./deps/zyre/src/.libs/libzyre.a  

zyre: deps/zyre

deps/zyre:
	cd deps && \
	tar zxvf ${ZYRE}.tar.gz && \
	ln -sf ${ZYRE} zyre && \
	cd ${ZYRE} && \
	./autogen.sh && \
	./configure --with-libzmq=`pwd`/../zeromq --with-libczmq=`pwd`/../czmq  && \
	make && \
	ln -s src/.libs lib

# ................ msgpack ................

CFLAGS_MSGPACK=-I./deps/msgpack/src
LDFLAGS_MSGPACK=./deps/msgpack/src/.libs/libmsgpack.a  

msgpack: deps/msgpack

deps/msgpack:
	cd deps && \
	tar zxvf ${MSGPACK}.tar.gz && \
	ln -sf ${MSGPACK} msgpack && \
	cd ${MSGPACK} && \
	./bootstrap && \
	./configure && \
	make 

# ................ liblfds ................

CFLAGS_LIBLFDS=-I./deps/liblfds/inc
LDFLAGS_LIBLFDS=./deps/liblfds/bin/liblfds611.a 

liblfds: deps/liblfds

deps/liblfds:
	cd deps && \
	tar zxvf ${LIBLFDS}.tar.gz && \
	ln -sf ${LIBLFDS} liblfds && \
	cd ${LIBLFDS} && \
	make -f makefile.linux

# ................ pcl ................

CFLAGS_PCL=-I./deps/pcl/include
LDFLAGS_PCL=./deps/pcl/pcl/.libs/libpcl.a  

pcl: deps/pcl

deps/pcl:
	cd deps && \
	tar zxvf ${PCL}.tar.gz && \
	ln -sf ${PCL} pcl && \
	cd ${PCL} && \
	./configure && \
	make 

# ................ colib ................

CFLAGS_COLIB=-I./deps/colib
LDFLAGS_COLIB=./deps/colib/lib/libcolib.a -lstdc++ -lpthread -ldl 

colib: deps/colib

deps/colib:
	cd deps && \
	tar zxvf ${COLIB}.tar.gz && \
	ln -sf ${COLIB} colib && \
	cd ${COLIB} && \
	make 

# ---------------- all ----------------
	
#CFLAGS_UCONTEXT=-D_XOPEN_SOURCE # ucontext.h error: The deprecated ucontext routines require _XOPEN_SOURCE to be defined.

COMMON_CFLAGS += ${CFLAGS_LIBUV} \
				 ${CFLAGS_JEMALLOC} \
				 ${CFLAGS_LEVELDB} \
				 ${CFLAGS_ROCKSDB} \
				 ${CFLAGS_LSM_SQLITE4} \
				 ${CFLAGS_LIBLMDB} \
				 ${CFLAGS_ZYRE} ${CFLAGS_CZMQ} ${CFLAGS_ZEROMQ} \
				 ${CFLAGS_MSGPACK} 
FINAL_CFLAGS = -Wstrict-prototypes \
			   ${COMMON_CFLAGS} \
			   ${CFLAGS}

#${CFLAGS_LIBLFDS} 
#${CFLAGS_PCL} 
#${CFLAGS_COLIB} 
#-DUSE_PRCTL
FINAL_CXXFLAGS=${COMMON_CFLAGS} ${CXXFLAGS}

FINAL_LDFLAGS = -static \
				${LDFLAGS_LIBUV} \
				${LDFLAGS_JEMALLOC} \
				${LDFLAGS_LEVELDB} \
				${LDFLAGS_ROCKSDB} \
				${LDFLAGS_LSM_SQLITE4} \
				${LDFLAGS_LIBLMDB} \
				${LDFLAGS_ZYRE} \
				${LDFLAGS_CZMQ} \
				${LDFLAGS_ZEROMQ} \
				${LDFLAGS_MSGPACK} \
				${LDFLAGS} -lpthread -lssl -lcrypto -lstdc++ -lm -lz

#${LDFLAGS_LIBLFDS} 
#${LDFLAGS_PCL} 
#${LDFLAGS_COLIB} 

ifeq (${UNAME}, Linux)
	FINAL_CFLAGS += -DOS_LINUX
	FINAL_LDFLAGS += /usr/lib/x86_64-linux-gnu/libuuid.a -lrt
endif
ifeq (${UNAME}, Darwin)
	FINAL_CFLAGS += -DOS_DARWIN
endif

#${SERVER}: deps ${BASE_OBJS} ${SERVER_OBJS} ${LEGOLAS_OBJS} bin
${SERVER}: ${BASE_OBJS} ${SERVER_OBJS} ${LEGOLAS_OBJS} ${KVDB_OBJS}  
	${CC} -o ${SERVER} \
		${BASE_OBJS} \
		${SERVER_OBJS} \
		${LEGOLAS_OBJS} \
		${KVDB_OBJS} \
		${FINAL_LDFLAGS} 


${CLIENT}: ${BASE_OBJS} ${CLIENT_OBJS} ${LEGOLAS_OBJS} 
	${CC} -o ${CLIENT} \
		${BASE_OBJS} \
		${CLIENT_OBJS} \
		${LEGOLAS_OBJS} \
		${FINAL_LDFLAGS}

bin:
	mkdir -p bin

%.oo: %.cpp
	${CC} ${FINAL_CXXFLAGS} -o $*.oo -c  $*.cpp

%.o: %.c
	${CC} ${FINAL_CFLAGS} -o $*.o -c  $*.c 

# ---------------- clean ----------------

clean:
	rm -fr \
		${SERVER} \
		${CLIENT} \
		${BASE_OBJS} \
		${SERVER_OBJS} \
		${CLIENT_OBJS} \
		${LEGOLAS_OBJS} \
		${LSM_OBJS} \
		${LEVELDB_OBJS} \
		${ROCKSDB_OBJS} \
		${KVDB_OBJS}

clean-deps:
	rm -fr \
		deps/${LIBUV} deps/libuv \
		deps/${JEMALLOC} deps/jemalloc \
		deps/${LEVELDB} deps/leveldb \
		deps/${LIBLMDB} deps/rocksdb \
		deps/${ROCKSDB} deps/liblmdb \
		deps/${LSM_SQLITE4} deps/lsm \
		deps/${ZEROMQ} deps/zeromq \
		deps/${CZMQ} deps/czmq \
		deps/${ZYRE} deps/zyre  

cleanall: clean clean-deps

# ---------------- run ----------------

run:
	${SERVER} -t

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
	nc localhost 16076

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

