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

    struct kvdb_t;

    struct kvdb_t *kvdb_open(const char *dbpath);
    void kvdb_close(struct kvdb_t *kvdb);

    int kvdb_put(struct kvdb_t *kvdb, const char *key, uint32_t keylen, const char *value, uint32_t valuelen);
    const char *kvdb_get(struct kvdb_t *kvdb, const char *key, uint32_t keylen);
    int kvdb_delete(struct kvdb_t *kvdb, const char *key, uint32_t keylen);


#ifdef __cplusplus
}
#endif

#endif /* __KVDB_H__ */

