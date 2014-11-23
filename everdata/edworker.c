/**
 * @file   edworker.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-08 03:51:23
 * 
 * @brief  
 * 
 * 
 */

#include <czmq.h>
#include "common.h"
#include "logger.h"
#include "everdata.h"
#include "vnode.h"

#include "datanode.h"

/* ================ get_storage_type_name() ================ */
const char *get_storage_type_name(int storage_type)
{
    if ( storage_type == STORAGE_NONE )
        return "NONE";
    if ( storage_type == STORAGE_LOGFILE )
        return "LOGFILE";
    if ( storage_type == STORAGE_KVDB_LMDB )
        return "LMDB";
    if ( storage_type == STORAGE_KVDB_EBLOB )
        return "EBLOB";
    if ( storage_type == STORAGE_KVDB_LEVELDB )
        return "LEVELDB";
    if ( storage_type == STORAGE_KVDB_ROCKSDB )
        return "ROCKSDB";
    if ( storage_type == STORAGE_KVDB_LSM )
        return "LSM-SQLITE4";

    return "Unknown";
}

/* ================ run_edworker_for_container() ================ */
int run_edworker(const char *broker_endpoint, uint32_t total_containers, uint32_t total_buckets, uint32_t total_channels, int storage_type, int verbose)
{
    info_log("run_edworker() with %d containers %d buckets %d channels connect to %s. Storage Type(%d):%s", total_containers, total_buckets, total_channels, broker_endpoint, storage_type, get_storage_type_name(storage_type));

    datanode_t *datanode = datanode_new(total_containers, total_buckets, total_channels, storage_type, broker_endpoint, verbose);

    datanode_loop(datanode);

    datanode_free(datanode);

    return 0;
}

