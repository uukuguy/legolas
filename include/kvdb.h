/**
 * @file  kvdb.h 
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-06-13 00:29:35
 * 
 * @brief  
 * 
 * 
 */

#ifndef __KVDB_H__
#define __KVDB_H__

#include "common.h"

#ifdef __cplusplus
extern "C" {
#endif

    typedef struct kvdb_t kvdb_t;

    typedef struct db_methods_t {
        void (*db_close)(kvdb_t *);
        int (*db_put)(kvdb_t *, const char *, uint32_t , void *, uint32_t);
        int (*db_get)(kvdb_t *, const char *, uint32_t, void **, uint32_t *);
        int (*db_del)(kvdb_t *, const char *, uint32_t);
        void (*db_flush)(kvdb_t *);
        //int (*xScan)(TestDb *, void *, int, void *, int, void *, int,
                //void (*)(void *, void *, int , void *, int)
                //);
        //int (*xDeleteRange)(TestDb *, void *, int, void *, int);
        int (*db_begin)(kvdb_t *, int);
        int (*db_commit)(kvdb_t *, int);
        int (*db_rollback)(kvdb_t *, int);
    } db_methods_t;

    typedef struct kvenv_t{
        const char *dbclass;
        const char *dbpath;
        uint32_t max_dbsize;
        uint32_t max_dbs;
    } kvenv_t;

    typedef struct kvdb_t {
        kvenv_t *kvenv;
        db_methods_t const *db_methods;
        const char *dbclass;
    } kvdb_t;


    kvenv_t *kvenv_new(const char *dbclass, const char *dbpath, uint64_t max_dbsize, uint32_t max_dbs);
    void kvenv_free(kvenv_t *kvenv);
    size_t kvenv_get_dbsize(kvenv_t *kvenv);

    kvdb_t *kvdb_open(kvenv_t *kvenv, const char *dbname);
    void kvdb_close(kvdb_t *kvdb);

    int kvdb_put(kvdb_t *kvdb, const char *key, uint32_t klen, void *value, uint32_t vlen);
    int kvdb_get(kvdb_t *kvdb, const char *key, uint32_t klen, void **value, uint32_t *vlen);
    int kvdb_del(kvdb_t *kvdb, const char *key, uint32_t klen);
    void kvdb_flush(kvdb_t *kvdb);

    void undefined_kvdb_function(kvdb_t *);
    int undefined_transaction_function(kvdb_t *, int);

    int kvdb_get_uint32(kvdb_t *kvdb, const char *key, uint32_t *ret_value);
    int kvdb_put_uint32(kvdb_t *kvdb, const char *key, uint32_t value);
    int kvdb_get_uint64(kvdb_t *kvdb, const char *key, uint64_t *ret_value);
    int kvdb_put_uint64(kvdb_t *kvdb, const char *key, uint64_t value);
    int kvdb_get_str(kvdb_t *kvdb, const char *key, char **value, uint32_t *value_len);
    int kvdb_put_str(kvdb_t *kvdb, const char *key, const char *value);
    int kvdb_get_data(kvdb_t *kvdb, const char *key, char **value, uint32_t *vlen);
    int kvdb_put_data(kvdb_t *kvdb, const char *key, const char *value, uint32_t vlen);

#ifdef __cplusplus
}
#endif

#endif /* __KVDB_H__ */

