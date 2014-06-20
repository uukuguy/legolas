HYPERDEX_ROOT=/home/jwsu/apps/legolas/hyperdex

LIBPO6=libpo6-0.5.0
LIBE=libe-0.7.0
BUSYBEE=busybee-0.5.0
HYPERLEVELDB=hyperleveldb-1.1.0
REPLICANT=replicant-0.6.0
HYPERDEX=hyperdex-1.3.0

.PHONY: deps

all: deps hyperdex

deps: libpo6 libe busybee hyperleveldb replicant

# ................ libpo6 ................
libpo6:
	tar zxvf ${LIBPO6}.tar.gz && \
	ln -sf ${LIBPO6} libpo6 && \
	cd ${LIBPO6} && \
	./configure --prefix=${HYPERDEX_ROOT} \
	--enable-shared=yes \
	&& \
	make

CFLAGS_LIBPO6=-I../libpo6

# ................ libe ................
libe:
	tar zxvf ${LIBE}.tar.gz && \
	ln -sf ${LIBE} libe && \
	cd ${LIBE} && \
	CXXFLAGS="${CFLAGS_LIBPO6}" \
	./configure --prefix=${HYPERDEX_ROOT} \
	--enable-shared=yes \
	&& \
	make

CFLAGS_LIBE=-I../libe
LDFLAGS_LIBE=-L../libe/.libs -le

# ................ busybee ................
busybee:
	tar zxvf ${BUSYBEE}.tar.gz && \
	ln -sf ${BUSYBEE} busybee && \
	patch -p0 < ./${BUSYBEE}.patch && \
	cd ${BUSYBEE} && \
	../../../autogen.sh && \
	CXXFLAGS="${CFLAGS_LIBPO6} ${CFLAGS_LIBE}" \
	./configure --prefix=${HYPERDEX_ROOT} \
	--enable-shared=yes \
	&& \
	make

CFLAGS_BUSYBEE=-I../busybee
LDFLAGS_BUSYBEE=-L../busybee/.libs -lbusybee


# ................ hyperleveldb ................
hyperleveldb:
	tar zxvf ${HYPERLEVELDB}.tar.gz && \
	ln -sf ${HYPERLEVELDB} hyperleveldb && \
	cd ${HYPERLEVELDB} && \
	./configure --prefix=${HYPERDEX_ROOT} \
	--enable-shared=yes \
	&& \
	make

CFLAGS_HYPERLEVELDB=-I../hyperleveldb
LDFLAGS_HYPERLEVELDB=-L../hyperleveldb/.libs -lhyperleveldb

# ................ replicant ................
replicant:
	tar zxvf ${REPLICANT}.tar.gz && \
	ln -sf ${REPLICANT} replicant && \
	patch -p0 < ./${REPLICANT}.patch && \
	cd ${REPLICANT} && \
	../../../autogen.sh && \
	CXXFLAGS="${CFLAGS_HYPERLEVELDB} ${CFLAGS_LIBPO6} ${CFLAGS_LIBE} ${CFLAGS_BUSYBEE}" \
	LDFLAGS="${LDFLAGS_HYPERLEVELDB} ${LDFLAGS_BUSYBEE} ${LDFLAGS_LIBE}" \
	./configure --prefix=${HYPERDEX_ROOT} \
	--enable-shared=yes \
	&& \
	make

CFLAGS_REPLICANT=-I../replicant/daemon -I../replicant/client
LDFLAGS_REPLICANT=-L../replicant/.libs -lreplicant-state-machine -lreplicant

# ................ hyperdex ................
hyperdex:
	tar zxvf ${HYPERDEX}.tar.gz && \
	ln -sf ${HYPERDEX} hyperdex && \
	patch -p0 < ./${HYPERDEX}.patch && \
	cd ${HYPERDEX} && \
	../../../autogen.sh && \
	CXXFLAGS="${CFLAGS_REPLICANT} ${CFLAGS_HYPERLEVELDB} ${CFLAGS_BUSYBEE} ${CFLAGS_LIBE} ${CFLAGS_LIBPO6}" \
	CFLAGS="${CFLAGS_REPLICANT} ${CFLAGS_HYPERLEVELDB} ${CFLAGS_BUSYBEE} ${CFLAGS_LIBE} ${CFLAGS_LIBPO6}" \
	LDFLAGS="-Wl,-rpath,../../lib ${LDFLAGS_REPLICANT} ${LDFLAGS_HYPERLEVELDB} ${LDFLAGS_BUSYBEE} ${LDFLAGS_LIBE} " \
	./configure --prefix=${HYPERDEX_ROOT} \
	--enable-shared=yes \
	--enable-ycsb \
	--enable-python-bindings \
	--enable-maintainer-tools \
	--enable-time-consuming-tests=no \
	&& \
	make

CFLAGS_HYPERDEX=-I../hyperdex/include
LDFLAGS_HYPERDEX=-L../hyperdex/.libs -lhyperdex-coordinator -lhyperdex-admin -lhyperdex-client

# ................ clean ................
clean: clean-libpo6 clean-libe clean-busybee clean-hyperleveldb clean-replicant clean-hyperdex

clean-libpo6:
	rm -fr ${LIBPO6} libpo6 
	
clean-libe:
	rm -fr ${LIBE} libe 
	
clean-busybee:
	rm -fr ${BUSYBEE} busybee 

clean-hyperleveldb:
	rm -fr ${HYPERLEVELDB} hyperleveldb 

clean-replicant:
	rm -fr ${REPLICANT} replicant

clean-hyperdex:
	rm -fr ${HYPERDEX} hyperdex

install:
	${MAKE} -C ${LIBPO6} install && \
	${MAKE} -C ${LIBE} install && \
	${MAKE} -C ${BUSYBEE} install && \
	${MAKE} -C ${HYPERLEVELDB} install && \
	${MAKE} -C ${REPLICANT} install && \
	${MAKE} -C ${HYPERDEX} install

