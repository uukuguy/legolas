TARGET=../bin/test_legolas

OBJS = test_main.cpp.o

FINAL_CXXFLAGS = -I../base -I../include -I../net
FINAL_LDFLAGS = -L../lib ../instrument/function.o ../base/dlist.o

.PHONY: ${TARGET}

all: ${TARGET}

%.cpp.o: %.cpp
	${CC} ${FINAL_CXXFLAGS} -o $*.cpp.o -c  $*.cpp

%.o: %.c
	${CC} ${FINAL_CFLAGS} -o $*.o -c  $*.c 


${TARGET}: ${OBJS}
	${CC} -o ${TARGET} ${OBJS} ${FINAL_LDFLAGS} -lgtest -lpthread -lstdc++

clean:
	rm -f ${OBJS} ${TARGET}

