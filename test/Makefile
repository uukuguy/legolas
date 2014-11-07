TARGET=../bin/test_legolas

OBJS = test_main.cc.o

FINAL_CXXFLAGS = -I../base -I../include -I../net
FINAL_LDFLAGS = -L../lib ../instrument/function.c.o ../base/dlist.c.o

.PHONY: ${TARGET}

all: ${TARGET}

%.cc.o: %.cc
	${CC} ${FINAL_CXXFLAGS} -o $*.cc.o -c  $*.cc

%.c.o: %.c
	${CC} ${FINAL_CFLAGS} -o $*.c.o -c  $*.c 


${TARGET}: ${OBJS}
	${CC} -o ${TARGET} ${OBJS} ${FINAL_LDFLAGS} -lgtest -lpthread -lstdc++

clean:
	rm -f ${OBJS} ${TARGET}

