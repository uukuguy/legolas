/**
 * @file  kvdb.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   22014-06-06 10:54:56
 * 
 * @brief  
 * 
 * 
 */

#include "common.h"
#include "kvdb.h"
#include "zmalloc.h"
#include "filesystem.h"

#ifdef HAS_LMDB
kvdb_t *kvdb_lmdb_open(const char *dbpath);
#endif

#ifdef HAS_LEVELDB
kvdb_t *kvdb_leveldb_open(const char *dbpath);
#endif

#ifdef HAS_ROCKSDB
kvdb_t *kvdb_rocksdb_open(const char *dbpath);
#endif

#ifdef HAS_LSM
kvdb_t *kvdb_lsm_open(const char *dbpath);
#endif

typedef struct kvdb_classes_t {
    const char *dbclass;
    kvdb_t *(*kvdb_open)(const char*);
} kvdb_classes_t;

static kvdb_classes_t kvdb_classes[] ={
#ifdef HAS_LMDB
    {"lmdb", kvdb_lmdb_open},
#endif
#ifdef HAS_LEVELDB
    {"leveldb", kvdb_leveldb_open},
#endif
#ifdef HAS_ROCKSDB
    {"rocksdb", kvdb_rocksdb_open},
#endif
#ifdef HAS_LSM
    {"lsm", kvdb_lsm_open},
#endif
};

kvdb_t *kvdb_open(const char *dbclass, const char *dbpath)
{
    mkdir_if_not_exist(dbpath);
    int i;
    for ( i = 0 ; i < sizeof(kvdb_classes) / sizeof(kvdb_classes_t) ; i++ ){
        if ( strcmp(dbclass, kvdb_classes[i].dbclass) == 0 ){
            char fullpath[NAME_MAX];
            sprintf(fullpath, "%s/%s", dbpath, kvdb_classes[i].dbclass);
            mkdir_if_not_exist(fullpath);
            return kvdb_classes[i].kvdb_open(fullpath);
        }
    }
/*#ifdef HAS_LEVELDB*/
    /*if ( strcmp(dbclass, "leveldb") == 0 ){*/
        /*return (kvdb_t*)kvdb_leveldb_open(dbpath);*/
    /*}*/
/*#endif*/

/*#ifdef HAS_ROCKSDB*/
    /*if ( strcmp(dbclass, "rocksdb") == 0 ){*/
        /*return (kvdb_t*)kvdb_rocksdb_open(dbpath);*/
    /*}*/
/*#endif*/

/*#ifdef HAS_LSM*/
    /*if ( strcmp(dbclass, "lsm") == 0 ){*/
        /*return (kvdb_t*)kvdb_lsm_open(dbpath);*/
    /*}*/
/*#endif*/

    return NULL;
}

void kvdb_close(kvdb_t *kvdb)
{
    if ( kvdb->db_methods->db_close != NULL ){
        kvdb->db_methods->db_close(kvdb);
    }
}

int kvdb_put(kvdb_t *kvdb, void *key, uint32_t klen, void *value, uint32_t vlen)
{
    if ( kvdb->db_methods->db_put != NULL ){
        return kvdb->db_methods->db_put(kvdb, key, klen, value, vlen);
    } else{
        return -1;
    }
}

int kvdb_get(kvdb_t *kvdb, void *key, uint32_t klen, void **value, uint32_t *vlen)
{
    if ( kvdb->db_methods->db_get != NULL ){
        return kvdb->db_methods->db_get(kvdb, key, klen, value, vlen);
    } else{
        return -1;
    }
}

int kvdb_del(kvdb_t *kvdb, void *key, uint32_t klen)
{
    if ( kvdb->db_methods->db_del != NULL ){
        return kvdb->db_methods->db_del(kvdb, key, klen);
    } else{
        return -1;
    }
}

void kvdb_flush(kvdb_t *kvdb)
{
    if ( kvdb->db_methods->db_flush != NULL ){
        kvdb->db_methods->db_flush(kvdb);
    }
}


void undefined_kvdb_function(kvdb_t *kvdb)
{
}

int undefined_transaction_function(kvdb_t *kvdb, int level)
{
    return -1;
}

