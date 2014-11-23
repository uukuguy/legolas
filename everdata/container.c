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

#include "bucket.h"
#include "container.h"

void container_thread_main(zsock_t *pipe, void *user_data)
{
    ZPIPE_ACTOR_THREAD_BEGIN(pipe);
    {

        container_t *container = (container_t*)user_data;
        trace_log("Container(%d) Ready.", container->id);

        ZPIPE_NEW_BEGIN(container, container->total_buckets);

        bucket_t *bucket = bucket_new(i, container, container->storage_type, container->broker_endpoint);

        ZPIPE_NEW_END(container, bucket);

        ZPIPE_LOOP(container);

        trace_log("Container(%d) Exit.", container->id);
    }
    ZPIPE_ACTOR_THREAD_END(pipe);

}

/* ================ container_new() ================ */
container_t *container_new(int container_id, int total_buckets, int storage_type, const char *broker_endpoint, int verbose)
{
    container_t *container = (container_t*)malloc(sizeof(container_t));
    memset(container, 0, sizeof(container_t));

    container->id = container_id;
    container->storage_type = storage_type;
    container->broker_endpoint = broker_endpoint;
    container->verbose = verbose;

    container->total_buckets = total_buckets;

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

