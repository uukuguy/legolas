/**
 * @file  kvdb_lmdb.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   22014-07-07 17:24:58
 * 
 * @brief  
 * 
 * 
 */

#include "lmdb.h"
#include "common.h"
#include "kvdb.h"
#include "zmalloc.h"
#include "logger.h"

typedef struct kvdb_lmdb_t {
    kvdb_t kvdb;
    MDB_env *env;
    MDB_dbi dbi;
} kvdb_lmdb_t;

void kvdb_lmdb_close(kvdb_t *kvdb);
int kvdb_lmdb_put(kvdb_t *kvdb, void *key, uint32_t klen, void *value, uint32_t vlen);
int kvdb_lmdb_get(kvdb_t *kvdb, void *key, uint32_t klen, void **ppVal, uint32_t *pnVal);
int kvdb_lmdb_del(kvdb_t *kvdb, void *key, uint32_t klen);
void kvdb_lmdb_flush(kvdb_t *kvdb);

static const db_methods_t lmdb_methods = {
    kvdb_lmdb_close,
    kvdb_lmdb_put,
    kvdb_lmdb_get,
    kvdb_lmdb_del,
    kvdb_lmdb_flush,
    undefined_transaction_function,
    undefined_transaction_function,
    undefined_transaction_function
};

kvdb_t *kvdb_lmdb_open(const char *dbpath)
{
    MDB_txn *txn;
    int maxreaders = 8;
    int maxdbs = 4;

    kvdb_lmdb_t *lmdb = (kvdb_lmdb_t *)zmalloc(sizeof(struct kvdb_lmdb_t));
    memset(lmdb, 0, sizeof(kvdb_lmdb_t));

    lmdb->kvdb.dbclass = "lmdb";
    lmdb->kvdb.db_methods = &lmdb_methods;

    int rc = mdb_env_create(&lmdb->env);
    if ( rc != 0 ) {
        zfree(lmdb);
        error_log("mdb_env_create() failed.");
        return NULL;
    }

    rc = mdb_env_set_mapsize(lmdb->env, 1*1024*1024*1024);
    if ( rc != 0 ) {
        zfree(lmdb);
        error_log("mdb_env_set_mapsize() failed.");
        return NULL;
    }

    rc = mdb_env_set_maxreaders(lmdb->env, maxreaders); 
    if ( rc != 0 ) {
        zfree(lmdb);
        error_log("mdb_env_set_maxreaders() failed.");
        return NULL;
    }

    rc = mdb_env_set_maxdbs(lmdb->env, maxdbs); 
    if ( rc != 0 ) {
        zfree(lmdb);
        error_log("mdb_env_set_maxdbs() failed.");
        return NULL;
    }

    /*rc = mdb_env_open(lmdb->env, dbpath, MDB_WRITEMAP | MDB_NOTLS, 0640); */
    /*rc = mdb_env_open(lmdb->env, dbpath, MDB_MAPASYNC | MDB_WRITEMAP | MDB_NOTLS, 0640); */
    /*rc = mdb_env_open(lmdb->env, dbpath, MDB_NOSYNC | MDB_WRITEMAP | MDB_NOTLS, 0640); */
    rc = mdb_env_open(lmdb->env, dbpath, MDB_FIXEDMAP | MDB_NOSYNC, 0640); 
    if ( rc != 0 ) {
        zfree(lmdb);
        error_log("mdb_env_open() failed.");
        return NULL;
    }

    rc = mdb_txn_begin(lmdb->env, NULL, 0, &txn);
    if ( rc != 0 ) {
        zfree(lmdb);
        error_log("mdb_txn_begin() failed.");
        return NULL;
    }

    rc = mdb_open(txn, NULL, 0, &lmdb->dbi);
    if ( rc != 0 ) {
        zfree(lmdb);
        error_log("mdb_open() failed.");
        return NULL;
    }

    rc = mdb_txn_commit(txn);
    if ( rc != 0 ) {
        zfree(lmdb);
        error_log("mdb_txn_commit() failed.");
        return NULL;
    }


    return (kvdb_t*)lmdb;
}

void kvdb_lmdb_close(kvdb_t *kvdb){
    kvdb_lmdb_t *lmdb = (kvdb_lmdb_t*)kvdb;


    mdb_close(lmdb->env, lmdb->dbi);
    mdb_env_close(lmdb->env);
    zfree(lmdb);
}

int kvdb_lmdb_put(kvdb_t *kvdb, void *key, uint32_t klen, void *value, uint32_t vlen)
{
    kvdb_lmdb_t *lmdb = (kvdb_lmdb_t*)kvdb;

    MDB_val m_val;
    MDB_val m_key;
    MDB_txn *txn;

    m_val.mv_size = vlen; 
    m_val.mv_data = value;
    m_key.mv_size = klen; 
    m_key.mv_data = key;

    int rc = mdb_txn_begin(lmdb->env, NULL, 0, &txn);
    if( rc==0 ){
        rc = mdb_put(txn, lmdb->dbi, &m_key, &m_val, 0);
        if( rc==0 ){
            rc = mdb_txn_commit(txn);
        }else{
            mdb_txn_abort(txn);
        }
    }

    return rc;
}

int kvdb_lmdb_get(kvdb_t *kvdb, void *key, uint32_t klen, void **ppVal, uint32_t *pnVal)
{
    kvdb_lmdb_t *lmdb = (kvdb_lmdb_t*)kvdb;

    MDB_val m_key;
    MDB_txn *txn;

    m_key.mv_size = klen;
    m_key.mv_data = key;

    int rc = mdb_txn_begin(lmdb->env, NULL, MDB_RDONLY, &txn);
    if( rc==0 ){
        MDB_val m_val = {0, 0};
        rc = mdb_get(txn, lmdb->dbi, &m_key, &m_val);
        if( rc==MDB_NOTFOUND ){
            rc = 0;
            *ppVal = 0;
            *pnVal = -1;
        }else{
            uint32_t nVal = m_val.mv_size;
            char *pVal = m_val.mv_data;
            char *result = (char*)zmalloc(nVal);
            memcpy(result, pVal, nVal);
            *ppVal = result;

            *pnVal = nVal;
        }
        mdb_txn_commit(txn);
    }

    return rc;
}

int kvdb_lmdb_del(kvdb_t *kvdb, void *key, uint32_t klen)
{
    kvdb_lmdb_t *lmdb = (kvdb_lmdb_t*)kvdb;

    MDB_val m_key;
    MDB_txn *txn;

    m_key.mv_size = klen; 
    m_key.mv_data = key;

    int rc = mdb_txn_begin(lmdb->env, NULL, 0, &txn);
    if( rc==0 ){
        rc = mdb_del(txn, lmdb->dbi, &m_key, 0);
        if( rc==0 ){
            rc = mdb_txn_commit(txn);
        }else{
            mdb_txn_abort(txn);
        }
    }

    return rc;
}

void kvdb_lmdb_flush(kvdb_t *kvdb)
{
    kvdb_lmdb_t *lmdb = (kvdb_lmdb_t*)kvdb;
    mdb_env_sync(lmdb->env, 1);
}

