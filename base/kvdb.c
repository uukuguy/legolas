/**
 * @file  kvdb.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-06-06 10:54:56
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
kvdb_t *kvdb_lmdb_open(kvenv_t *kvenv, const char *dbname);

kvenv_t *kvenv_new_lmdb(const char *dbpath, uint64_t max_dbsize, uint32_t max_dbs);
void kvenv_free_lmdb(kvenv_t *kvenv);
size_t kvenv_get_dbsize_lmdb(kvenv_t *kvenv);

#endif

#ifdef HAS_LEVELDB
kvdb_t *kvdb_leveldb_open(kvenv_t *kvenv, const char *dbname);
#endif

#ifdef HAS_ROCKSDB
kvdb_t *kvdb_rocksdb_open(kvenv_t *kvenv, const char *dbname);
#endif

#ifdef HAS_LSM
kvdb_t *kvdb_lsm_open(kvenv_t *kvenv, const char *dbname);
#endif

#ifdef HAS_EBLOB
kvdb_t *kvdb_eblob_open(kvenv_t *kvenv, const char *dbname);
#endif

typedef struct kvdb_classes_t {
    const char *dbclass;
    kvdb_t *(*kvdb_open)(kvenv_t*, const char *dbname);
    kvenv_t *(*kvenv_new)(const char *, uint64_t, uint32_t);
    void (*kvenv_free)(kvenv_t *);
    size_t (*kvenv_get_dbsize)(kvenv_t *);
} kvdb_classes_t;

static kvdb_classes_t kvdb_classes[] ={
#ifdef HAS_LMDB
    {"lmdb", kvdb_lmdb_open, kvenv_new_lmdb, kvenv_free_lmdb, kvenv_get_dbsize_lmdb},
#endif
#ifdef HAS_LEVELDB
    {"leveldb", kvdb_leveldb_open, NULL, NULL, NULL},
#endif
#ifdef HAS_ROCKSDB
    {"rocksdb", kvdb_rocksdb_open, NULL, NULL, NULL},
#endif
#ifdef HAS_LSM
    {"lsm", kvdb_lsm_open, NULL, NULL, NULL},
#endif
#ifdef HAS_EBLOB
    {"eblob", kvdb_eblob_open, NULL, NULL, NULL}, 
#endif
};

kvenv_t *kvenv_new(const char *dbclass, const char *dbpath, uint64_t max_dbsize, uint32_t max_dbs)
{
    kvenv_t *kvenv = NULL;

    mkdir_if_not_exist(dbpath);
    int i;
    for ( i = 0 ; i < sizeof(kvdb_classes) / sizeof(kvdb_classes_t) ; i++ ){
        if ( strcmp(dbclass, kvdb_classes[i].dbclass) == 0 ){
            char fullpath[NAME_MAX];
            sprintf(fullpath, "%s/%s", dbpath, kvdb_classes[i].dbclass);
            mkdir_if_not_exist(fullpath);
            if ( kvdb_classes[i].kvenv_new != NULL ){
                kvenv = kvdb_classes[i].kvenv_new(fullpath, max_dbsize, max_dbs);
            } else {
                kvenv = (kvenv_t*)zmalloc(sizeof(kvenv_t));
                memset(kvenv, 0, sizeof(kvenv_t));
                kvenv->dbclass = dbclass;
                kvenv->dbpath = fullpath;
                kvenv->max_dbsize = max_dbsize;
                kvenv->max_dbs = max_dbs;
            }
            break;
        }
    }

    assert(kvenv != NULL);
    return kvenv;
}

void kvenv_free(kvenv_t *kvenv)
{
    int i;
    for ( i = 0 ; i < sizeof(kvdb_classes) / sizeof(kvdb_classes_t) ; i++ ){
        if ( strcmp(kvenv->dbclass, kvdb_classes[i].dbclass) == 0 ){
            if ( kvdb_classes[i].kvenv_free != NULL ){
                kvdb_classes[i].kvenv_free(kvenv);
            } else {
                zfree(kvenv);
            }
            break;
        } 
    }
}


size_t kvenv_get_dbsize(kvenv_t *kvenv)
{
    size_t dbsize = 0;

    int i;
    for ( i = 0 ; i < sizeof(kvdb_classes) / sizeof(kvdb_classes_t) ; i++ ){
        if ( strcmp(kvenv->dbclass, kvdb_classes[i].dbclass) == 0 ){
            if ( kvdb_classes[i].kvenv_get_dbsize != NULL ){
                dbsize = kvdb_classes[i].kvenv_get_dbsize(kvenv);
            } else {
                dbsize = 0;
            }
            break;
        } 
    }

    return dbsize;
}

kvdb_t *kvdb_open(kvenv_t *kvenv, const char *dbname)
{
    const char *dbclass = kvenv->dbclass;
    const char *dbpath = kvenv->dbpath;

    mkdir_if_not_exist(dbpath);
    int i;
    for ( i = 0 ; i < sizeof(kvdb_classes) / sizeof(kvdb_classes_t) ; i++ ){
        if ( strcmp(dbclass, kvdb_classes[i].dbclass) == 0 ){
            return kvdb_classes[i].kvdb_open(kvenv, dbname);
        }
    }

    return NULL;
}

void kvdb_close(kvdb_t *kvdb)
{
    if ( kvdb->db_methods->db_close != NULL ){
        kvdb->db_methods->db_close(kvdb);
    }
}

int kvdb_put(kvdb_t *kvdb, const char *key, uint32_t klen, void *value, uint32_t vlen)
{
    if ( kvdb->db_methods->db_put != NULL ){
        return kvdb->db_methods->db_put(kvdb, key, klen, value, vlen);
    } else{
        return -1;
    }
}

int kvdb_get(kvdb_t *kvdb, const char *key, uint32_t klen, void **value, uint32_t *vlen)
{
    if ( kvdb->db_methods->db_get != NULL ){
        return kvdb->db_methods->db_get(kvdb, key, klen, value, vlen);
    } else{
        return -1;
    }
}

int kvdb_del(kvdb_t *kvdb, const char *key, uint32_t klen)
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

int kvdb_get_uint32(kvdb_t *kvdb, const char *key, uint32_t *ret_value)
{
    int rc = -1;
    uint32_t *value = NULL;
    uint32_t value_len = 0;
    if ( kvdb_get(kvdb, key, strlen(key), (void**)&value, &value_len) == 0 ){
        if ( value != NULL && value_len == sizeof(uint32_t)){
            *ret_value = *value;
            rc = 0;
        }
    }
    return rc;
}

int kvdb_put_uint32(kvdb_t *kvdb, const char *key, uint32_t value)
{
    return kvdb_put(kvdb, key, strlen(key), &value, sizeof(uint32_t));
}

int kvdb_get_uint64(kvdb_t *kvdb, const char *key, uint64_t *ret_value)
{
    int rc = -1;
    uint64_t *value = NULL;
    uint32_t value_len = 0;
    if ( kvdb_get(kvdb, key, strlen(key), (void**)&value, &value_len) == 0 ){
        if ( value != NULL && value_len == sizeof(uint32_t)){
            *ret_value = *value;
            rc = 0;
        }
    }
    return rc;
}

int kvdb_put_uint64(kvdb_t *kvdb, const char *key, uint64_t value)
{
    return kvdb_put(kvdb, key, strlen(key), &value, sizeof(uint64_t));
}

int kvdb_get_str(kvdb_t *kvdb, const char *key, char **value, uint32_t *value_len)
{
    return kvdb_get(kvdb, key, strlen(key), (void**)value, value_len);
}

int kvdb_put_str(kvdb_t *kvdb, const char *key, const char *value)
{
    return kvdb_put(kvdb, key, strlen(key), &value, strlen(value));
}

int kvdb_get_data(kvdb_t *kvdb, const char *key, char **value, uint32_t *vlen)
{
    return kvdb_get(kvdb, key, strlen(key), (void**)value, vlen);
}

int kvdb_put_data(kvdb_t *kvdb, const char *key, const char *value, uint32_t vlen)
{
    return kvdb_put(kvdb, key, strlen(key), (void*)value, vlen);
}


