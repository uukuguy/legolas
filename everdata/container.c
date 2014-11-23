/**
 * @file   container.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-21 15:25:18
 * 
 * @brief  
 * 
 * 
 */

#include <czmq.h>
#include "common.h"
#include "filesystem.h"
#include "vnode.h"
#include "logger.h"
#include "md5.h"
#include "adlist.h"
#include "everdata.h"
#include "work.h"

#include "datanode.h"
#include "container.h"
#include "bucket.h"

void container_thread_main(zsock_t *pipe, void *user_data)
{
    ZPIPE_ACTOR_THREAD_BEGIN(pipe);
    {

        container_t *container = (container_t*)user_data;
        trace_log("Container(%d) Ready.", container->id);

        ZPIPE_NEW_BEGIN(container, container->total_buckets);

        bucket_t *bucket = bucket_new(container, i);

        ZPIPE_NEW_END(container, bucket);

        ZPIPE_LOOP(container);

        trace_log("Container(%d) Exit.", container->id);
    }
    ZPIPE_ACTOR_THREAD_END(pipe);

}

/* ================ container_new() ================ */
container_t *container_new(datanode_t *datanode, uint32_t container_id)
{
    container_t *container = (container_t*)malloc(sizeof(container_t));
    memset(container, 0, sizeof(container_t));

    container->id = container_id;
    container->datanode = datanode;

    container->storage_type = datanode->storage_type;
    container->broker_endpoint = datanode->broker_endpoint;
    container->verbose = datanode->verbose;
    container->total_buckets = datanode->total_buckets;
    container->total_channels = datanode->total_channels;

    const char *storage_dir = "./data/storage";
    if ( mkdir_if_not_exist(storage_dir) != 0 ){
        error_log("mkdir %s failed.", storage_dir);
        abort();
    }

    sprintf(container->data_dir, "%s/%04d", storage_dir, container_id);
    if ( mkdir_if_not_exist(container->data_dir) != 0 ){
        error_log("mkdir %s failed.", container->data_dir);
        abort();
    }


    container->heartbeat_at = zclock_time() + HEARTBEAT_INTERVAL;

    ZPIPE_ACTOR_NEW(container, container_thread_main);

    return container;
}

/* ================ container_free() ================ */
void container_free(container_t *container)
{
    ZPIPE_FREE(container, bucket_free);

    ZPIPE_ACTOR_FREE(container);

    free(container);
}

