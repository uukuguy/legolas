
TARGET = bin/rsm_test
RSM_LIB= lib/librsm.a

MAIN_OBJS = src/main.cc.o

CLIENT_OBJS = Client/Client.cc.o \
			  Client/ClientImpl.cc.o \
			  Client/ClientImplBase.cc.o

RSM_OBJS = src/Consensus.cc.o \
		   src/RaftConsensus.cc.o \
		   src/StateMachine.cc.o \
		   src/RaftConsensusInvariants.cc.o 

		   #src/Globals.cc.o

CORE_OBJS = Core/Checksum.cc.o \
			Core/Config.cc.o \
			Core/Debug.cc.o \
			Core/ProtoBuf.cc.o \
			Core/Random.cc.o \
			Core/StringUtil.cc.o \
			Core/ThreadId.cc.o \
			Core/Buffer.cc.o

STORAGE_OBJS = Storage/FilesystemUtil.cc.o \
			   Storage/Log.cc.o \
			   Storage/LogFactory.cc.o \
			   Storage/MemoryLog.cc.o \
			   Storage/SimpleFileLog.cc.o \
			   Storage/SnapshotFile.cc.o

PROTOBUF_SRC = ./Protocol/Raft.pb.h ./Protocol/Raft.pb.cc \
				./Protocol/RaftLogMetadata.pb.h ./Protocol/RaftLogMetadata.pb.cc \
				./Protocol/SimpleFileLog.pb.h ./Protocol/SimpleFileLog.pb.cc \
				./Protocol/SnapshotStats.pb.h ./Protocol/SnapshotStats.pb.cc \
				./Protocol/SnapshotMetadata.pb.h ./Protocol/SnapshotMetadata.pb.cc \
				./Protocol/Client.pb.h ./Protocol/Client.pb.cc \
				./Protocol/Sessions.pb.h ./Protocol/Sessions.pb.cc

PROTOBUF_OBJS = ./Protocol/Raft.pb.cc.o \
				./Protocol/RaftLogMetadata.pb.cc.o \
				./Protocol/SimpleFileLog.pb.cc.o \
				./Protocol/SnapshotStats.pb.cc.o \
				./Protocol/SnapshotMetadata.pb.cc.o \
				./Protocol/Client.pb.cc.o \
				./Protocol/Sessions.pb.cc.o

LIB_OBJS = ${CLIENT_OBJS} ${RSM_OBJS} ${STORAGE_OBJS} ${CORE_OBJS}  ${PROTOBUF_OBJS}

all: bin lib ${PROTOBUF_SRC} ${RSM_LIB} ${TARGET}

bin:
	mkdir -p bin

lib:
	mkdir -p lib

${PROTOBUF_SRC}:
	protoc --cpp_out . ./Protocol/Raft.proto
	protoc --cpp_out . ./Protocol/RaftLogMetadata.proto
	protoc --cpp_out . ./Protocol/SimpleFileLog.proto
	protoc --cpp_out . ./Protocol/SnapshotStats.proto
	protoc --cpp_out . ./Protocol/SnapshotMetadata.proto
	protoc --cpp_out . ./Protocol/Client.proto
	protoc --cpp_out . ./Protocol/Sessions.proto

${LIB_OBJS}:%.cc.o:%.cc
	gcc -fPIC -m64 -Wall -Werror -std=c++11 -I. -c -o $@ $<

${MAIN_OBJS}:%.cc.o:%.cc
	gcc -m64 -Wall -Werror -std=c++11 -I. -c -o $@ $<

${RSM_LIB}: ${LIB_OBJS} 
	ar curv ${RSM_LIB} ${LIB_OBJS} 

${TARGET}: ${LIB_OBJS} ${MAIN_OBJS}
clean:
	rm -f ${TARGET} ${RSM_LIB} ${LIB_OBJS} ${MAIN_OBJS} ${PROTOBUF_SRC} ${PROTOBUF_OBJS}




