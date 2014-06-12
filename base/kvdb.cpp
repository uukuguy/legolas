/**
 * @file  kvdb.h 
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-06-13 00:29:35
 * 
 * @brief  
 * 
 * 
 */

#include <leveldb/db.h>

#include "common.h"
#include "kvdb.h"
#include "zmalloc.h"

typedef struct kvdb_t {
    leveldb::DB *db;
} kvdb_t;

struct kvdb_t *kvdb_open(const char *dbpath)
{
    kvdb_t *kvdb = (kvdb_t *)zmalloc(sizeof(struct kvdb_t));

    leveldb::Options options;
    options.create_if_missing = true;
    leveldb::Status status = leveldb::DB::Open(options, dbpath, &kvdb->db); 
    if ( status.ok() ){
        return kvdb;
    } else {
        return NULL;
    }
}

void kvdb_close(struct kvdb_t *kvdb)
{
    delete kvdb->db;
    kvdb->db = NULL;
}

int kvdb_put(struct kvdb_t *kvdb, const char *key, const char *value)
{
    leveldb::Status status = kvdb->db->Put(leveldb::WriteOptions(), key, value);
    if ( status.ok() ){
        return 0;
    } else {
        return -1;
    }
}

const char *kvdb_get(struct kvdb_t *kvdb, const char *key)
{
    std::string value;
    leveldb::Status status = kvdb->db->Get(leveldb::ReadOptions(), key, &value);
    if ( status.ok() ){
        return value.c_str();
    } else {
        return NULL;
    }
}

int kvdb_delete(struct kvdb_t *kvdb, const char *key)
{
    leveldb::Status status = kvdb->db->Delete(leveldb::WriteOptions(), key);
    if ( status.ok() ){
        return 0;
    } else {
        return -1;
    }
}

