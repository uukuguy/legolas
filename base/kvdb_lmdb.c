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

typedef struct kvenv_lmdb_t{
    kvenv_t kvenv;
    MDB_env *env;
} kvenv_lmdb_t;

typedef struct kvdb_lmdb_t {
    kvdb_t kvdb;
    MDB_dbi dbi;
} kvdb_lmdb_t;

void kvdb_lmdb_close(kvdb_t *kvdb);
int kvdb_lmdb_put(kvdb_t *kvdb, const char *key, uint32_t klen, void *value, uint32_t vlen);
int kvdb_lmdb_get(kvdb_t *kvdb, const char *key, uint32_t klen, void **ppVal, uint32_t *pnVal);
int kvdb_lmdb_del(kvdb_t *kvdb, const char *key, uint32_t klen);
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

kvenv_t *kvenv_new_lmdb(const char *dbpath, uint64_t max_dbsize, uint32_t max_dbs)
{
    kvenv_lmdb_t *kvenv_lmdb = (kvenv_lmdb_t*)zmalloc(sizeof(kvenv_lmdb_t));
    memset(kvenv_lmdb, 0, sizeof(kvenv_lmdb_t));
    kvenv_lmdb->kvenv.dbclass = "lmdb";
    kvenv_lmdb->kvenv.max_dbsize = max_dbsize;
    kvenv_lmdb->kvenv.max_dbs = max_dbs;

    int rc = mdb_env_create(&kvenv_lmdb->env);
    if ( rc != 0 ) {
        zfree(kvenv_lmdb);
        error_log("mdb_env_create() failed.");
        return NULL;
    }

    int maxreaders = 256;
    if ( max_dbs == 0 ){
        max_dbs = 256;
    }
    if ( max_dbsize == 0 ) {
        max_dbsize = 1024L * 1024L * 1024L * 4L;
    }

    rc = mdb_env_set_mapsize(kvenv_lmdb->env, max_dbsize);
    if ( rc != 0 ) {
        zfree(kvenv_lmdb);
        error_log("mdb_env_set_mapsize() failed.");
        return NULL;
    }

    rc = mdb_env_set_maxreaders(kvenv_lmdb->env, maxreaders); 
    if ( rc != 0 ) {
        zfree(kvenv_lmdb);
        error_log("mdb_env_set_maxreaders() failed.");
        return NULL;
    }

    rc = mdb_env_set_maxdbs(kvenv_lmdb->env, max_dbs); 
    if ( rc != 0 ) {
        zfree(kvenv_lmdb);
        error_log("mdb_env_set_maxdbs() failed.");
        return NULL;
    }

    /*rc = mdb_env_open(lmdb->env, dbpath, MDB_FIXEDMAP | MDB_NOSYNC, 0640); */

    /*rc = mdb_env_open(lmdb->env, dbpath, MDB_MAPASYNC | MDB_WRITEMAP | MDB_NOTLS , 0640); */
    /*rc = mdb_env_open(kvenv_lmdb->env, dbpath, MDB_MAPASYNC | MDB_WRITEMAP, 0640); */
    /*rc = mdb_env_open(kvenv_lmdb->env, dbpath, MDB_NOMETASYNC, 0640); */
    rc = mdb_env_open(kvenv_lmdb->env, dbpath, MDB_NOSYNC, 0640); 
    if ( rc != 0 ) {
        zfree(kvenv_lmdb);
        error_log("mdb_env_open() failed. dbpath=%s error: %s", dbpath, mdb_strerror(rc));
        return NULL;
    }

    return (kvenv_t*)kvenv_lmdb;
}

void kvenv_free_lmdb(kvenv_t *kvenv)
{
    kvenv_lmdb_t *kvenv_lmdb = (kvenv_lmdb_t*)kvenv;
    mdb_env_close(kvenv_lmdb->env);

    zfree(kvenv);
}

size_t kvenv_get_dbsize_lmdb(kvenv_t *kvenv)
{
    kvenv_lmdb_t *kvenv_lmdb = (kvenv_lmdb_t*)kvenv;

    MDB_envinfo stat;
    mdb_env_info(kvenv_lmdb->env, &stat);
    return 0x1000L * stat.me_last_pgno;
}

kvdb_t *kvdb_lmdb_open(kvenv_t *kvenv, const char *dbname)
{
    MDB_txn *txn;

    kvdb_lmdb_t *lmdb = (kvdb_lmdb_t *)zmalloc(sizeof(struct kvdb_lmdb_t));
    memset(lmdb, 0, sizeof(kvdb_lmdb_t));

    lmdb->kvdb.kvenv = kvenv;
    lmdb->kvdb.dbclass = "lmdb";
    lmdb->kvdb.db_methods = &lmdb_methods;

    kvenv_lmdb_t *kvenv_lmdb = (kvenv_lmdb_t*)kvenv;

    int rc = mdb_txn_begin(kvenv_lmdb->env, NULL, 0, &txn);
    if ( rc != 0 ) {
        zfree(lmdb);
        error_log("mdb_txn_begin() failed.");
        return NULL;
    }

    rc = mdb_open(txn, dbname, MDB_CREATE, &lmdb->dbi);
    if ( rc != 0 ) {
        zfree(lmdb);
        error_log("mdb_open() failed. dbname:%s", dbname);
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
    kvenv_lmdb_t *kvenv_lmdb = (kvenv_lmdb_t*)kvdb->kvenv;

    mdb_close(kvenv_lmdb->env, lmdb->dbi);
    zfree(lmdb);
}

int kvdb_lmdb_put(kvdb_t *kvdb, const char *key, uint32_t klen, void *value, uint32_t vlen)
{
    kvdb_lmdb_t *lmdb = (kvdb_lmdb_t*)kvdb;
    kvenv_lmdb_t *kvenv_lmdb = (kvenv_lmdb_t*)kvdb->kvenv;

    MDB_val m_val;
    MDB_val m_key;
    MDB_txn *txn;

    m_val.mv_size = vlen; 
    m_val.mv_data = value;
    m_key.mv_size = klen; 
    m_key.mv_data = (void*)key;

    int rc = mdb_txn_begin(kvenv_lmdb->env, NULL, 0, &txn);
    if( rc==0 ){
        rc = mdb_put(txn, lmdb->dbi, &m_key, &m_val, 0);
        if( rc==0 ){
            rc = mdb_txn_commit(txn);
        }else{
            error_log("kvdb_lmdb_put() failure. error: %s", mdb_strerror(rc));
            mdb_txn_abort(txn);
        }
    }

    return rc;
}

int kvdb_lmdb_get(kvdb_t *kvdb, const char *key, uint32_t klen, void **ppVal, uint32_t *pnVal)
{
    kvdb_lmdb_t *lmdb = (kvdb_lmdb_t*)kvdb;
    kvenv_lmdb_t *kvenv_lmdb = (kvenv_lmdb_t*)kvdb->kvenv;

    MDB_val m_key;
    MDB_txn *txn;

    m_key.mv_size = klen;
    m_key.mv_data = (void*)key;

    int rc = mdb_txn_begin(kvenv_lmdb->env, NULL, MDB_RDONLY, &txn);
    if( rc==0 ){
        MDB_val m_val = {0, 0};
        rc = mdb_get(txn, lmdb->dbi, &m_key, &m_val);
        if ( rc == 0 ) {
            uint32_t nVal = m_val.mv_size;
            char *pVal = m_val.mv_data;
            char *result = (char*)zmalloc(nVal);
            memcpy(result, pVal, nVal);
            *ppVal = result;
            *pnVal = nVal;
        } else if( rc==MDB_NOTFOUND ){
            rc = 0;
            *ppVal = 0;
            *pnVal = -1;
        }else{
            error_log("kvdb_lmdb_get() failure. error: %s", mdb_strerror(rc));
        }
        mdb_txn_commit(txn);
    }

    return rc;
}

int kvdb_lmdb_del(kvdb_t *kvdb, const char *key, uint32_t klen)
{
    kvdb_lmdb_t *lmdb = (kvdb_lmdb_t*)kvdb;
    kvenv_lmdb_t *kvenv_lmdb = (kvenv_lmdb_t*)kvdb->kvenv;

    MDB_val m_key;
    MDB_txn *txn;

    m_key.mv_size = klen; 
    m_key.mv_data = (void*)key;

    int rc = mdb_txn_begin(kvenv_lmdb->env, NULL, 0, &txn);
    if( rc==0 ){
        rc = mdb_del(txn, lmdb->dbi, &m_key, 0);
        if( rc==0 ){
            rc = mdb_txn_commit(txn);
        }else{
            error_log("kvdb_lmdb_del() failure. error: %s", mdb_strerror(rc));
            mdb_txn_abort(txn);
        }
    }

    return rc;
}

void kvdb_lmdb_flush(kvdb_t *kvdb)
{
    /*kvdb_lmdb_t *lmdb = (kvdb_lmdb_t*)kvdb;*/
    kvenv_lmdb_t *kvenv_lmdb = (kvenv_lmdb_t*)kvdb->kvenv;
    mdb_env_sync(kvenv_lmdb->env, 1);
}

