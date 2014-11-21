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

/* ================ handle_pullin_on_bucket_pipe() ================ */
int handle_pullin_on_bucket_pipe(zloop_t *loop, zsock_t *pipe, void *user_data)
{
    bucket_t *bucket = (bucket_t*)user_data;
    container_t *container = bucket->container;

    if ( container->total_over_buckets >= container->total_buckets ){
        zloop_reader_end(loop, pipe);
        return -1;
    }

    zmsg_t *msg = zmsg_recv(pipe);
    if ( msg == NULL ){
        zloop_reader_end(loop, pipe);
        return -1;
    }
    /*zmsg_print(msg);*/

    if ( message_check_status(msg, MSG_STATUS_ACTOR_OVER) == 0 ){
        container->total_over_buckets++;
        notice_log("Bucket actor %d over! (%d/%d)", bucket->id, container->total_over_buckets, container->total_buckets);
    }

    zmsg_destroy(&msg);

    return 0;
}

void container_thread_main(zsock_t *pipe, void *user_data)
{
    // zactor pattern
    zsock_signal(pipe, 0);
    message_send_status(pipe, MSG_STATUS_ACTOR_READY);
    // zactor pattern

    container_t *container = (container_t*)user_data;
    trace_log("Container(%d) Ready.", container->id);

    int verbose = container->verbose;
    zloop_t *loop = zloop_new();
    zloop_set_verbose(loop, verbose);

    uint32_t total_buckets = container->total_buckets;

    for ( int i = 0 ; i < total_buckets ; i++ ){
        zactor_t *actor = container->buckets[i]->actor;
        zloop_reader(loop, (zsock_t*)zactor_resolve(actor), handle_pullin_on_bucket_pipe, container->buckets[i]);
    }

    zloop_start(loop);

    zloop_destroy(&loop);

    trace_log("Container(%d) Exit.", container->id);
}

/* ================ container_new() ================ */
container_t *container_new(int container_id, int total_buckets, int storage_type, const char *broker_endpoint, int verbose)
{
    container_t *container = (container_t*)malloc(sizeof(container_t));
    memset(container, 0, sizeof(container_t));

    container->id = container_id;
    container->storage_type = storage_type;
    container->total_buckets = total_buckets;
    container->broker_endpoint = broker_endpoint;
    container->verbose = verbose;

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

    container->buckets = (bucket_t**)malloc(sizeof(bucket_t*) * total_buckets);
    for ( int i = 0 ; i < container->total_buckets ; i++ ){
        container->buckets[i] = bucket_new(i, container, container->storage_type, container->broker_endpoint);
    }

    container->heartbeat_at = zclock_time() + HEARTBEAT_INTERVAL;

    container->actor = zactor_new(container_thread_main, container);

    return container;
}

/* ================ container_free() ================ */
void container_free(container_t *container)
{
    zactor_destroy(&container->actor);
    container->actor = NULL;

    for ( int i = 0 ; i < container->total_buckets ; i++ ){
        bucket_free(container->buckets[i]);
        container->buckets[i] = NULL;
    }
    free(container->buckets);
    container->buckets = NULL;

    free(container);
}

