/**
 * @file  kvdb_lsm.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-06-06 15:10:09
 * 
 * @brief  
 * 
 * 
 */

#include "lsm.h"
#include "common.h"
#include "kvdb.h"
#include "zmalloc.h"

#define LSMTEST_DFLT_MT_MAX_CKPT (8*1024)
#define LSMTEST_DFLT_MT_MIN_CKPT (2*1024)

typedef struct kvdb_lsm_t {
    kvdb_t kvdb;
    lsm_env env;
    lsm_db *db;
    int nBuf;
    int nSector;
} kvdb_lsm_t;

void kvdb_lsm_close(kvdb_t *kvdb);
int kvdb_lsm_put(kvdb_t *kvdb, const char *key, uint32_t klen, void *value, uint32_t vlen);
int kvdb_lsm_get(kvdb_t *kvdb, const char *key, uint32_t klen, void **ppVal, uint32_t *pnVal);
int kvdb_lsm_del(kvdb_t *kvdb, const char *key, uint32_t klen);

static const db_methods_t lsm_methods = {
    kvdb_lsm_close,
    kvdb_lsm_put,
    kvdb_lsm_get,
    kvdb_lsm_del,
    undefined_kvdb_function,
    undefined_transaction_function,
    undefined_transaction_function,
    undefined_transaction_function
};

lsm_env *global_lsm_env(void)
{
  static int bInit = 0;
  static lsm_env env;
  if( bInit==0 ){
    memcpy(&env, lsm_default_env(), sizeof(env));
    bInit = 1;
  }
  return &env;
}

kvdb_t *kvdb_lsm_open(kvenv_t *kvenv, const char *dbname)
{
    const char *dbpath = kvenv->dbpath;

    kvdb_lsm_t *lsm = (kvdb_lsm_t *)zmalloc(sizeof(struct kvdb_lsm_t));
    memset(lsm, 0, sizeof(kvdb_lsm_t));

    lsm->kvdb.kvenv = kvenv;
    lsm->kvdb.dbclass = "lsm";
    lsm->kvdb.db_methods = &lsm_methods;

    lsm->nSector = 512;
    memcpy(&lsm->env, global_lsm_env(), sizeof(lsm_env));

    int rc = lsm_new(&lsm->env, &lsm->db);
    if ( rc == 0 ){
        char fullpath[NAME_MAX];
        sprintf(fullpath, "%s/data", dbpath);
        rc = lsm_open(lsm->db, fullpath);
        if ( rc == 0 ) {
            return (kvdb_t*)lsm;
        } else {
            kvdb_lsm_close((kvdb_t*)lsm);
        }
    }

    return NULL;
}

void kvdb_lsm_close(kvdb_t *kvdb){
  kvdb_lsm_t *lsm = (kvdb_lsm_t*)kvdb;

  /*lsm_csr_close(lsm->pCsr);*/
  lsm_close(lsm->db);

  zfree(lsm);

}

int kvdb_lsm_put(kvdb_t *kvdb, const char *key, uint32_t klen, void *value, uint32_t vlen)
{
  kvdb_lsm_t *lsm = (kvdb_lsm_t*)kvdb;

  int rc = lsm_write(lsm->db, (void*)key, klen, value, vlen);

  return rc;
}

int kvdb_lsm_get(kvdb_t *kvdb, const char *key, uint32_t klen, void **ppVal, uint32_t *pnVal)
{
  kvdb_lsm_t *lsm = (kvdb_lsm_t*)kvdb;
  lsm_cursor *csr;

  int rc = lsm_csr_open(lsm->db, &csr);
  if ( rc != 0 ) return -1;

  rc = lsm_csr_seek(csr, (void*)key, klen, LSM_SEEK_EQ);
  if( rc == 0 ){
    if( lsm_csr_valid(csr) ){
        void *pVal; int nVal;
        rc = lsm_csr_value(csr, &pVal, &nVal);

        char *result = (char*)zmalloc(nVal);
        memcpy(result, pVal, nVal);
        *ppVal = result;

        *pnVal = nVal;
    }else{
        *ppVal = NULL;
        *pnVal = -1;
    }
  }

  lsm_csr_close(csr);

  return rc;
}

int kvdb_lsm_del(kvdb_t *kvdb, const char *key, uint32_t klen)
{
  kvdb_lsm_t *lsm = (kvdb_lsm_t*)kvdb;

  return lsm_delete(lsm->db, (void*)key, klen);
}

