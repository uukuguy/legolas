/**
 * @file  kvdb_eblob.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-10-10 20:37:32
 * 
 * @brief  
 * 
 * 
 */

#include "eblob/blob.h"
#include "common.h"
#include "kvdb.h"
#include "zmalloc.h"
#include "logger.h"

typedef struct kvdb_eblob_t {
    kvdb_t kvdb;
    struct eblob_log	logger_;
    struct eblob_backend *eblob_;
    char *filename;
} kvdb_eblob_t;

void kvdb_eblob_close(kvdb_t *kvdb);
int kvdb_eblob_put(kvdb_t *kvdb, const char *key, uint32_t klen, void *value, uint32_t vlen);
int kvdb_eblob_get(kvdb_t *kvdb, const char *key, uint32_t klen, void **ppVal, uint32_t *pnVal);
int kvdb_eblob_del(kvdb_t *kvdb, const char *key, uint32_t klen);
void kvdb_eblob_flush(kvdb_t *kvdb);

static const db_methods_t eblob_methods = {
    kvdb_eblob_close,
    kvdb_eblob_put,
    kvdb_eblob_get,
    kvdb_eblob_del,
    kvdb_eblob_flush,
    undefined_transaction_function,
    undefined_transaction_function,
    undefined_transaction_function
};

kvdb_t *kvdb_eblob_open(const char *dbpath)
{
    kvdb_eblob_t *eblob = (kvdb_eblob_t *)zmalloc(sizeof(struct kvdb_eblob_t));
    memset(eblob, 0, sizeof(kvdb_eblob_t));

    eblob->kvdb.dbclass = "eblob";
    eblob->kvdb.db_methods = &eblob_methods;

    eblob->logger_.log_private = fopen("/dev/stdout", "a");
    eblob->logger_.log_level = EBLOB_LOG_ERROR;
    eblob->logger_.log = eblob_log_raw_formatted;

	struct eblob_config cfg;
	memset(&cfg, 0, sizeof(cfg));

    eblob->filename = zmalloc(NAME_MAX);
    sprintf(eblob->filename, "%s/blob", dbpath);
	cfg.file = eblob->filename;
	cfg.log = &eblob->logger_;
	cfg.sync = 30;
    cfg.blob_size = 500 * 1024 * 1024;

	eblob->eblob_ = eblob_init(&cfg);
	if ( eblob->eblob_ == NULL ) {
        fclose(eblob->logger_.log_private);
        zfree(eblob);
        error_log("eblob_init() failed.");
        return NULL;
    }

    return (kvdb_t*)eblob;
}

void kvdb_eblob_close(kvdb_t *kvdb){
    kvdb_eblob_t *eblob = (kvdb_eblob_t*)kvdb;

	eblob_cleanup(eblob->eblob_);
    fclose(eblob->logger_.log_private);

    zfree(eblob->filename);

    zfree(eblob);
}

int kvdb_eblob_put(kvdb_t *kvdb, const char *key, uint32_t klen, void *value, uint32_t vlen)
{
    kvdb_eblob_t *eblob = (kvdb_eblob_t*)kvdb;

    int flags = BLOB_DISK_CTL_APPEND;
	int rc = eblob_write(eblob->eblob_, (struct eblob_key *)key, (void *)value, 0, vlen, flags);
    if ( rc != 0 ) {
        error_log("eblob_write() failed. rc=%d err=%s", rc, strerror(-rc));
    }

    return rc;
}

int kvdb_eblob_get(kvdb_t *kvdb, const char *key, uint32_t klen, void **ppVal, uint32_t *pnVal)
{
    kvdb_eblob_t *eblob = (kvdb_eblob_t*)kvdb;

	char *data;
	uint64_t dsize = 64 * 1024;
    int rc = eblob_read_data_nocsum(eblob->eblob_, (struct eblob_key *)key, 0, &data, &dsize);

    char *result = (char*)zmalloc(dsize);
    memcpy(result, data, dsize);
    *ppVal = result;

    return rc;
}

int kvdb_eblob_del(kvdb_t *kvdb, const char *key, uint32_t klen)
{
    kvdb_eblob_t *eblob = (kvdb_eblob_t*)kvdb;

    int rc = 0;

	eblob_remove(eblob->eblob_, (struct eblob_key *)key);

    return rc;
}

void kvdb_eblob_flush(kvdb_t *kvdb)
{
    /*kvdb_eblob_t *eblob = (kvdb_eblob_t*)kvdb;*/

}

