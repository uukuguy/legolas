SERVER = bin/legolasd
CLIENT = bin/legolas

LEGOLAS_OBJS = legolas/legolas.o legolas/protocol.o 

BASE_OBJS = base/logger.o base/daemon.o base/coroutine.o
BASE_OBJS += base/zmalloc.o base/work.o base/md5.o base/filesystem.o
BASE_OBJS += base/adlist.o
#BASE_OBJS += base/lockfree_queue.o
#BASE_OBJS += base/lockfree_queue.o base/lockfree_reapd.o base/hazard_ptr.o

SERVER_OBJS = server/main.o server/server.o server/session.o server/session_recv.o server/session_send.o server/operation.o server/storage.o server/vnode.o server/datazone.o

CLIENT_OBJS = client/main.o client/client.o


LIBUV=libuv-v0.11.22
JEMALLOC=jemalloc-3.6.0
PCL=pcl-1.12
COLIB=colib-20140530
LIBLFDS=liblfds-6.1.1

UNAME := $(shell uname)

.PHONY: server client deps data

all: server client

server: ${SERVER}

client: ${CLIENT}

# ---------------- deps ----------------
.PHONY: jemalloc libuv liblfds pcl colib

deps: jemalloc libuv 
#pcl 
#colib

# ................ jemalloc ................

CFLAGS_JEMALLOC=-DUSE_JEMALLOC -DJEMALLOC_NO_DEMANGLE -I./deps/jemalloc/include
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

FINAL_CFLAGS = -ggdb -fPIC -m64 -Wall -Wstrict-prototypes  -D_GNU_SOURCE -I./include -I./legolas ${CFLAGS_UCONTEXT} ${CFLAGS_LIBUV} ${CFLAGS_JEMALLOC} ${CFLAGS}
#${CFLAGS_LIBLFDS} 
#${CFLAGS_PCL} 
#${CFLAGS_COLIB} 
#-DUSE_PRCTL
FINAL_LDFLAGS = ${LDFLAGS_LIBUV} ${LDFLAGS_JEMALLOC} ${LDFLAGS} -lpthread 
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

${SERVER}: deps ${BASE_OBJS} ${SERVER_OBJS} ${LEGOLAS_OBJS} bin
	${CC} -o ${SERVER} ${BASE_OBJS} ${SERVER_OBJS} ${LEGOLAS_OBJS} ${FINAL_LDFLAGS}


${CLIENT}: deps ${BASE_OBJS} ${CLIENT_OBJS} ${LEGOLAS_OBJS} bin
	${CC} -o ${CLIENT} ${BASE_OBJS} ${CLIENT_OBJS} ${LEGOLAS_OBJS} ${FINAL_LDFLAGS}

bin:
	mkdir -p bin

%.o: %.c
	${CC} ${FINAL_CFLAGS} -o $*.o -c  $*.c 

# ---------------- clean ----------------

clean:
	rm -fr ${SERVER} ${CLIENT} ${BASE_OBJS} ${SERVER_OBJS} ${CLIENT_OBJS} ${LEGOLAS_OBJS} 

clean-deps:
	rm -fr deps/${LIBUV} deps/libuv deps/${JEMALLOC} deps/jemalloc deps/${PCL} deps/pcl

cleanall: clean clean-deps

# ---------------- run ----------------

run:
	${SERVER} -t

# ---------------- test ----------------

data:
	mkdir -p data
	dd if=/dev/urandom of=data/320.dat bs=320 count=1
	dd if=/dev/urandom of=data/640.dat bs=640 count=1
	dd if=/dev/urandom of=data/1K.dat bs=1K count=1
	dd if=/dev/urandom of=data/32K.dat bs=1K count=32
	dd if=/dev/urandom of=data/64K.dat bs=1K count=64
	dd if=/dev/urandom of=data/100K.dat bs=1K count=100
	dd if=/dev/urandom of=data/320K.dat bs=1K count=320
	dd if=/dev/urandom of=data/640K.dat bs=1K count=640
	dd if=/dev/urandom of=data/1M.dat bs=1M count=1
	dd if=/dev/urandom of=data/2M.dat bs=1M count=2

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

