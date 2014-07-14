/**
 * @file  kvdb_leveldb.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-06-13 00:29:35
 * 
 * @brief  
 * 
 * 
 */

#include <leveldb/c.h>

#include "common.h"
#include "kvdb.h"
#include "zmalloc.h"

typedef struct kvdb_leveldb_t {
    kvdb_t kvdb;
    leveldb_t *db; 
    leveldb_options_t *pOpt;
    leveldb_writeoptions_t *pWriteOpt;
    leveldb_readoptions_t *pReadOpt;
    char *pVal;
} kvdb_leveldb_t;

void kvdb_leveldb_close(kvdb_t *kvdb);
int kvdb_leveldb_put(kvdb_t *kvdb, void *key, uint32_t klen, void *value, uint32_t vlen);
int kvdb_leveldb_get(kvdb_t *kvdb, void *key, uint32_t klen, void **ppVal, uint32_t *pnVal);
int kvdb_leveldb_del(kvdb_t *kvdb, void *key, uint32_t klen);

static const db_methods_t leveldb_methods = {
    kvdb_leveldb_close,
    kvdb_leveldb_put,
    kvdb_leveldb_get,
    kvdb_leveldb_del,
    undefined_transaction_function,
    undefined_transaction_function,
    undefined_transaction_function
};

kvdb_t *kvdb_leveldb_open(const char *dbpath)
{
    kvdb_leveldb_t *leveldb = (kvdb_leveldb_t *)zmalloc(sizeof(struct kvdb_leveldb_t));
    memset(leveldb, 0, sizeof(kvdb_leveldb_t));

    leveldb->kvdb.dbclass = "leveldb";
    leveldb->kvdb.db_methods = &leveldb_methods;

    leveldb->pOpt = leveldb_options_create();
    leveldb_options_set_create_if_missing(leveldb->pOpt, 1);
    leveldb->pWriteOpt = leveldb_writeoptions_create();
    leveldb->pReadOpt = leveldb_readoptions_create();

    char *szErr = NULL;
    leveldb->db = leveldb_open(leveldb->pOpt, dbpath, &szErr);

    if( szErr ){
        kvdb_leveldb_close((kvdb_t*)leveldb);
        return NULL;
    }

    return (kvdb_t*)leveldb;
}

void kvdb_leveldb_close(kvdb_t *kvdb){
  kvdb_leveldb_t *leveldb = (kvdb_leveldb_t*)kvdb;

  leveldb_close(leveldb->db);
  leveldb_writeoptions_destroy(leveldb->pWriteOpt);
  leveldb_readoptions_destroy(leveldb->pReadOpt);
  leveldb_options_destroy(leveldb->pOpt);
  if ( leveldb->pVal != NULL ){
      zfree(leveldb->pVal);
  }
  zfree(kvdb);

}

int kvdb_leveldb_put(kvdb_t *kvdb, void *key, uint32_t klen, void *value, uint32_t vlen)
{
  kvdb_leveldb_t *leveldb = (kvdb_leveldb_t*)kvdb;
  char *szErr = NULL;
  leveldb_put(leveldb->db, leveldb->pWriteOpt, (const char*)key, klen, (const char *)value, vlen, &szErr);
  if ( szErr ) {
      return -1;
  } else {
      return 0;
  }
}

int kvdb_leveldb_get(kvdb_t *kvdb, void *key, uint32_t klen, void **ppVal, uint32_t *pnVal)
{
  kvdb_leveldb_t *leveldb = (kvdb_leveldb_t*)kvdb;
  char *szErr = NULL;
  size_t nVal = 0;

  if ( leveldb->pVal != NULL ){
      zfree(leveldb->pVal);
      leveldb->pVal = NULL;
  }

  leveldb->pVal = leveldb_get(leveldb->db, leveldb->pReadOpt, (const char *)key, klen, &nVal, &szErr);
  *ppVal = (void *)(leveldb->pVal);
  if( leveldb->pVal == NULL ){
    *pnVal = -1;
  }else{
    *pnVal = nVal;
  }

  if ( szErr ) {
      return -1;
  } else {
      return 0;
  }
}

int kvdb_leveldb_del(kvdb_t *kvdb, void *key, uint32_t klen)
{
  kvdb_leveldb_t *leveldb = (kvdb_leveldb_t*)kvdb;
  char *szErr = NULL;

  leveldb_delete(leveldb->db, leveldb->pWriteOpt, (const char*)key, klen, &szErr);

  if ( szErr ) {
      return -1;
  } else {
      return 0;
  }
}

