
TARGET = bin/rsm_test
RSM_LIB= lib/librsm.a

MAIN_OBJS = src/main.oo
RSM_OBJS = src/Consensus.oo \
		   src/RaftConsensus.oo \
		   src/StateMachine.oo \
		   src/RaftConsensusInvariants.oo 

		   #src/Globals.oo

CORE_OBJS = Core/Checksum.oo \
			Core/Config.oo \
			Core/Debug.oo \
			Core/ProtoBuf.oo \
			Core/Random.oo \
			Core/StringUtil.oo \
			Core/ThreadId.oo \
			Core/Buffer.oo

STORAGE_OBJS = Storage/FilesystemUtil.oo \
			   Storage/Log.oo \
			   Storage/LogFactory.oo \
			   Storage/MemoryLog.oo \
			   Storage/SimpleFileLog.oo \
			   Storage/SnapshotFile.oo

PROTOBUF_SRC = ./Protocol/Raft.pb.h ./Protocol/Raft.pb.cc \
				./Protocol/RaftLogMetadata.pb.h ./Protocol/RaftLogMetadata.pb.cc \
				./Protocol/SimpleFileLog.pb.h ./Protocol/SimpleFileLog.pb.cc \
				./Protocol/SnapshotStats.pb.h ./Protocol/SnapshotStats.pb.cc \
				./Protocol/SnapshotMetadata.pb.h ./Protocol/SnapshotMetadata.pb.cc \
				./Protocol/Client.pb.h ./Protocol/Client.pb.cc \
				./Protocol/Sessions.pb.h ./Protocol/Sessions.pb.cc

PROTOBUF_OBJS = ./Protocol/Raft.pb.oo \
				./Protocol/RaftLogMetadata.pb.oo \
				./Protocol/SimpleFileLog.pb.oo \
				./Protocol/SnapshotStats.pb.oo \
				./Protocol/SnapshotMetadata.pb.oo \
				./Protocol/Client.pb.oo \
				./Protocol/Sessions.pb.oo

LIB_OBJS = ${RSM_OBJS} ${STORAGE_OBJS} ${CORE_OBJS}  ${PROTOBUF_OBJS}

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

${LIB_OBJS}:%.oo:%.cc
	gcc -fPIC -m64 -Wall -Werror -std=c++11 -I. -c -o $@ $<

${MAIN_OBJS}:%.oo:%.cc
	gcc -m64 -Wall -Werror -std=c++11 -I. -c -o $@ $<

${RSM_LIB}: ${LIB_OBJS} 
	ar curv ${RSM_LIB} ${LIB_OBJS} 

${TARGET}: ${LIB_OBJS} ${MAIN_OBJS}
clean:
	rm -f ${TARGET} ${RSM_LIB} ${LIB_OBJS} ${MAIN_OBJS} ${PROTOBUF_SRC} ${PROTOBUF_OBJS}




