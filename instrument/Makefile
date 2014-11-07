# --------------------------------------------------
 # @file   Makefile
 # @author Jiangwen Su <uukuguy@gmail.com>
 # @date   2014-11-05 11:06:05
 # 
 # @brief  
 #      INSTRUMENT_CFLAGS += -finstrument-functions
 #      INSTRUMENT_LDFLAGS += -linstrument
 # 		make dot
# --------------------------------------------------

TARGET=libinstrument.a
INSTRUMENT_OBJS = instrument.c.o function.c.o calltree.c.o
INSTRUMENT_FLAGS = -finstrument-functions

FINAL_CFLAGS = -g -fPIC -std=gnu11 -Wall ${INSTRUMENT_FLAGS} -I../include

all: ${TARGET}


${TARGET}: ${INSTRUMENT_OBJS}
	ar curv ${TARGET} ${INSTRUMENT_OBJS} && \
		cp ${TARGET} ../lib/

%.c.o: %.c
	${CC} ${FINAL_CFLAGS} -o $*.c.o -c  $*.c 

clean:
	rm -f ${TARGET} ${INSTRUMENT_OBJS}
