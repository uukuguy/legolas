/**
 * @file  kvdb_rocksdb.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-06-06 16:53:13
 * 
 * @brief  
 * 
 * 
 */

#include <rocksdb/c.h>

#include "common.h"
#include "kvdb.h"
#include "zmalloc.h"

typedef struct kvdb_rocksdb_t {
    kvdb_t kvdb;
    rocksdb_t *db; 
    rocksdb_options_t *pOpt;
    rocksdb_writeoptions_t *pWriteOpt;
    rocksdb_readoptions_t *pReadOpt;
} kvdb_rocksdb_t;

void kvdb_rocksdb_close(kvdb_t *kvdb);
int kvdb_rocksdb_put(kvdb_t *kvdb, void *key, uint32_t klen, void *value, uint32_t vlen);
int kvdb_rocksdb_get(kvdb_t *kvdb, void *key, uint32_t klen, void **ppVal, uint32_t *pnVal);
int kvdb_rocksdb_del(kvdb_t *kvdb, void *key, uint32_t klen);

static const db_methods_t rocksdb_methods = {
    kvdb_rocksdb_close,
    kvdb_rocksdb_put,
    kvdb_rocksdb_get,
    kvdb_rocksdb_del,
    undefined_kvdb_function,
    undefined_transaction_function,
    undefined_transaction_function,
    undefined_transaction_function
};

kvdb_t *kvdb_rocksdb_open(const char *dbpath)
{
    kvdb_rocksdb_t *rocksdb = (kvdb_rocksdb_t *)zmalloc(sizeof(struct kvdb_rocksdb_t));
    memset(rocksdb, 0, sizeof(kvdb_rocksdb_t));

    rocksdb->kvdb.dbclass = "rocksdb";
    rocksdb->kvdb.db_methods = &rocksdb_methods;

    rocksdb->pOpt = rocksdb_options_create();
    rocksdb_options_set_create_if_missing(rocksdb->pOpt, 1);
    rocksdb->pWriteOpt = rocksdb_writeoptions_create();
    rocksdb->pReadOpt = rocksdb_readoptions_create();

    char *szErr = NULL;
    rocksdb->db = rocksdb_open(rocksdb->pOpt, dbpath, &szErr);

    if( szErr ){
        kvdb_rocksdb_close((kvdb_t*)rocksdb);
        return NULL;
    }

    return (kvdb_t*)rocksdb;
}

void kvdb_rocksdb_close(kvdb_t *kvdb){
  kvdb_rocksdb_t *rocksdb = (kvdb_rocksdb_t*)kvdb;

  rocksdb_close(rocksdb->db);
  rocksdb_writeoptions_destroy(rocksdb->pWriteOpt);
  rocksdb_readoptions_destroy(rocksdb->pReadOpt);
  rocksdb_options_destroy(rocksdb->pOpt);
  zfree(kvdb);

}

int kvdb_rocksdb_put(kvdb_t *kvdb, void *key, uint32_t klen, void *value, uint32_t vlen)
{
  kvdb_rocksdb_t *rocksdb = (kvdb_rocksdb_t*)kvdb;
  char *szErr = NULL;
  rocksdb_put(rocksdb->db, rocksdb->pWriteOpt, (const char*)key, klen, (const char *)value, vlen, &szErr);
  if ( szErr ) {
      return -1;
  } else {
      return 0;
  }
}

int kvdb_rocksdb_get(kvdb_t *kvdb, void *key, uint32_t klen, void **ppVal, uint32_t *pnVal)
{
  kvdb_rocksdb_t *rocksdb = (kvdb_rocksdb_t*)kvdb;
  char *szErr = NULL;
  size_t nVal = 0;

  char *pVal = rocksdb_get(rocksdb->db, rocksdb->pReadOpt, (const char *)key, klen, &nVal, &szErr);

  if( pVal == NULL ){
    *pnVal = -1;
    return -1;
  }else{
    *pnVal = nVal;
  }

  if ( szErr ) {
      /*zfree(pVal);*/
      return -1;
  } else {
     char *result = (char*)zmalloc(nVal);
     memcpy(result, pVal, nVal);
     *ppVal = result;
     /*zfree(pVal);*/

      return 0;
  }
}

int kvdb_rocksdb_del(kvdb_t *kvdb, void *key, uint32_t klen)
{
  kvdb_rocksdb_t *rocksdb = (kvdb_rocksdb_t*)kvdb;
  char *szErr = NULL;

  rocksdb_delete(rocksdb->db, rocksdb->pWriteOpt, (const char*)key, klen, &szErr);

  if ( szErr ) {
      return -1;
  } else {
      return 0;
  }
}

