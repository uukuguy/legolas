TARGET = ../bin/edbroker

OBJS = main.c.o edbroker.c.o

COMMON_CFLAGS = -g -fPIC -Wall -Werror -I../deps/zeromq/include -I../deps/czmq/include -I../include

FINAL_CFLAGS = -std=gnu11 ${COMMON_CFLAGS}
FINAL_CXXFLAGS = -std=c++11 ${COMMON_CFLAGS}

COMMON_LDFLAGS = ../base/logger.c.o ../base/daemon.c.o 
FINAL_LDFLAGS = -static ${COMMON_LDFLAGS} -L../lib -lczmq -lzmq -lpthread -lstdc++

all: ${TARGET}

${TARGET}: ${OBJS}
	${CC} -o ${TARGET} ${OBJS} ${FINAL_LDFLAGS} 

%.c.o: %.c
	${CC} ${FINAL_CFLAGS} -o $*.c.o -c  $*.c

clean:
	rm -f ${TARGET} ${OBJS}

