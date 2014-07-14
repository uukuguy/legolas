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
        int (*db_put)(kvdb_t *, void *, uint32_t , void *, uint32_t);
        int (*db_get)(kvdb_t *, void *, uint32_t, void **, uint32_t *);
        int (*db_del)(kvdb_t *, void *, uint32_t);
        //int (*xScan)(TestDb *, void *, int, void *, int, void *, int,
                //void (*)(void *, void *, int , void *, int)
                //);
        //int (*xDeleteRange)(TestDb *, void *, int, void *, int);
        int (*db_begin)(kvdb_t *, int);
        int (*db_commit)(kvdb_t *, int);
        int (*db_rollback)(kvdb_t *, int);
    } db_methods_t;

    typedef struct kvdb_t {
        db_methods_t const *db_methods;
        const char *dbclass;
    } kvdb_t;

    kvdb_t *kvdb_open(const char *dbclass, const char *dbpath);
    void kvdb_close(kvdb_t *kvdb);

    int kvdb_put(kvdb_t *kvdb, void *key, uint32_t klen, void *value, uint32_t vlen);
    int kvdb_get(kvdb_t *kvdb, void *key, uint32_t klen, void **value, uint32_t *vlen);
    int kvdb_del(kvdb_t *kvdb, void *key, uint32_t klen);

    int undefined_transaction_function(kvdb_t *, int);


#ifdef __cplusplus
}
#endif

#endif /* __KVDB_H__ */

