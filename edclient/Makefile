TARGET = ../bin/edclient

OBJS = main.c.o edclient.c.o

COMMON_CFLAGS = -g -fPIC -Wall -Werror -I../deps/zeromq/include -I../deps/czmq/include -I../include

FINAL_CFLAGS = -std=gnu11 ${COMMON_CFLAGS}
FINAL_CXXFLAGS = -std=c++11 ${COMMON_CFLAGS}

BASE_LDFLAGS = -lbase -llmdb -leblob -leblob_react -lreact -lrocksdb -lleveldb -lsnappy -llsm-sqlite4 -lczmq -lzmq -lmsgpack -lbz2 -ljemalloc -lpthread -lssl -lcrypto -lstdc++ -lm -lz
COMMON_LDFLAGS = -L../lib ${BASE_LDFLAGS}
FINAL_LDFLAGS = -static ${COMMON_LDFLAGS} 

all: ${TARGET}

${TARGET}: ${OBJS}
	${CC} -o ${TARGET} ${OBJS} ${FINAL_LDFLAGS} 

%.c.o: %.c
	${CC} ${FINAL_CFLAGS} -o $*.c.o -c  $*.c

clean:
	rm -f ${TARGET} ${OBJS}

