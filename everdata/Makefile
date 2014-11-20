
EVERDATA_OBJS = everdata.c.o

EDBROKER = ../bin/edbroker
EDBROKER_OBJS = edbroker_main.c.o edbroker.c.o

EDWORKER = ../bin/edworker
EDWORKER_OBJS = edworker_main.c.o edworker.c.o

EDCLIENT = ../bin/edclient
EDCLIENT_OBJS = edclient_main.c.o edclient.c.o

EDFS = ../bin/edfs
EDFS_OBJS = edfs.c.o

LIBBASE = ../lib/libbase.a
LIBSERVER_OBJS = ../server/vnode.c.o ../server/datazone.c.o ../server/object.c.o 

COMMON_CFLAGS = -g -fPIC -Wall -Werror -I../deps/ccl -I../deps/zeromq/include -I../deps/czmq/include -I../include -I../server

FINAL_CFLAGS = -std=gnu11 ${COMMON_CFLAGS} 
FINAL_CXXFLAGS = -std=c++11 ${COMMON_CFLAGS}

BASE_LDFLAGS = -lccl -lbase -llmdb -leblob -leblob_react -lreact -lrocksdb -lleveldb -lsnappy -llsm-sqlite4 -lczmq -lzmq -lmsgpack -lbz2 -ljemalloc -lpthread -lssl -lcrypto -lstdc++ -lm -lz
COMMON_LDFLAGS = -L../lib ${BASE_LDFLAGS}
FINAL_LDFLAGS = ${COMMON_LDFLAGS} 

all: ${EDBROKER} ${EDWORKER} ${EDCLIENT} ${EDFS}

${EDBROKER}: ${LIBBASE} ${EDBROKER_OBJS} ${EVERDATA_OBJS}
	${CC} -static -o ${EDBROKER} ${EDBROKER_OBJS} ${EVERDATA_OBJS} ${FINAL_LDFLAGS} 

${EDWORKER}: ${LIBBASE} ${LIBSERVER_OBJS} ${EDWORKER_OBJS} ${EVERDATA_OBJS}
	${CC} -static -o ${EDWORKER} ${EDWORKER_OBJS} ${LIBSERVER_OBJS} ${EVERDATA_OBJS} ${FINAL_LDFLAGS} 

${EDCLIENT}: ${LIBBASE} ${EDCLIENT_OBJS} ${EVERDATA_OBJS}
	${CC} -static -o ${EDCLIENT} ${EDCLIENT_OBJS} ${EVERDATA_OBJS} ${FINAL_LDFLAGS} 

${EDFS}: edclient.c.o ${LIBBASE} ${EDFS_OBJS} ${EVERDATA_OBJS}
	${CC} -static -o ${EDFS} ${EDFS_OBJS} edclient.c.o ${EVERDATA_OBJS} -lfuse -ldl ${FINAL_LDFLAGS}

%.c.o: %.c
	${CC} ${FINAL_CFLAGS} -o $*.c.o -c  $*.c

clean:
	rm -f ${EVERDATA_OBJS} ${EDBROKER} ${EDBROKER_OBJS} ${EDWORKER} ${EDWORKER_OBJS} ${EDCLIENT} ${EDCLIENT_OBJS}

