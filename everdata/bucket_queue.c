/**
 * @file   bucket_queue.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-11 16:58:05
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

typedef struct write_ctx_t {
    zsock_t *sock;
    zframe_t *identity;
    vnode_t *vnode;
    object_t *object;
} write_ctx_t;

typedef struct read_ctx_t {
    zsock_t *sock;
    zframe_t *identity;
} read_ctx_t;

typedef struct delete_ctx_t {
    zsock_t *sock;
    zframe_t *identity;
} delete_ctx_t;

/* ================ write_queue_callback() ================ */
void write_queue_callback(work_queue_t *wq)
{
    write_ctx_t *wctx = NULL;
    while ( (wctx = (write_ctx_t*)dequeue_work(wq)) != NULL ){
        /*vnode_t *vnode = wctx->vnode;*/
        object_t *object = wctx->object;

        /*int rc = vnode_write_to_storage(vnode, object);*/
        int rc = 0;
        object_free(object);

        zmsg_t * sendback_msg = NULL;
        if ( rc == 0 ) 
            sendback_msg = create_status_message(MSG_STATUS_WORKER_ACK);
        else 
            sendback_msg = create_status_message(MSG_STATUS_WORKER_ERROR);

        zmsg_wrap(sendback_msg, wctx->identity);
        zmsg_send(&sendback_msg, wctx->sock);
    }
}

/* ================ read_queue_callback() ================ */
void read_queue_callback(work_queue_t *wq)
{
    read_ctx_t *rctx = NULL;
    while ( (rctx = (read_ctx_t*)dequeue_work(wq)) != NULL ){
        int rc = -1;

        zmsg_t * sendback_msg = NULL;
        if ( rc == 0 ) 
            sendback_msg = create_status_message(MSG_STATUS_WORKER_ACK);
        else 
            sendback_msg = create_status_message(MSG_STATUS_WORKER_ERROR);

        zmsg_wrap(sendback_msg, rctx->identity);
        zmsg_send(&sendback_msg, rctx->sock);
    }
}

/* ================ delete_queue_callback() ================ */
void delete_queue_callback(work_queue_t *wq)
{
    delete_ctx_t *dctx = NULL;
    while ( (dctx = (delete_ctx_t*)dequeue_work(wq)) != NULL ){
        int rc = -1;

        zmsg_t * sendback_msg = NULL;
        if ( rc == 0 ) 
            sendback_msg = create_status_message(MSG_STATUS_WORKER_ACK);
        else 
            sendback_msg = create_status_message(MSG_STATUS_WORKER_ERROR);

        zmsg_wrap(sendback_msg, dctx->identity);
        zmsg_send(&sendback_msg, dctx->sock);
    }
}

/* ================ write_ctx_new() ================ */
write_ctx_t *write_ctx_new(zsock_t *sock, zframe_t *identity, vnode_t *vnode, object_t *object)
{
    write_ctx_t *wctx = (write_ctx_t*)malloc(sizeof(write_ctx_t));

    wctx->sock = sock;
    wctx->identity = identity;
    wctx->vnode = vnode;
    wctx->object = object;

    return wctx;
}

/* ================ write_ctx_free() ================ */
void writer_ctx_free(write_ctx_t *wctx)
{
    free(wctx);
}

/* ================ read_ctx_new() ================ */
read_ctx_t *read_ctx_new(zsock_t *sock, zframe_t *identity)
{
    read_ctx_t *rctx = (read_ctx_t*)malloc(sizeof(read_ctx_t));

    rctx->sock = sock;
    rctx->identity = identity;

    return rctx;
}

/* ================ read_ctx_free() ================ */
void read_ctx_free(read_ctx_t *rctx)
{
    free(rctx);
}

/* ================ delete_ctx_new() ================ */
delete_ctx_t *delete_ctx_new(zsock_t *sock, zframe_t *identity)
{
    delete_ctx_t *dctx = (delete_ctx_t*)malloc(sizeof(delete_ctx_t));

    dctx->sock = sock;
    dctx->identity = identity;

    return dctx;
}

/* ================ delete_ctx_free() ================ */
void delete_ctx_free(delete_ctx_t *dctx)
{
    free(dctx);
}

/* ================ bucket_enqueue_write_queue() ================ */
void bucket_enqueue_write_queue(bucket_t *bucket, write_ctx_t *wctx)
{
    enqueue_work(bucket->write_queue, (void*)wctx);
}

/* ================ bucket_enqueue_read_queue() ================ */
void bucket_enqueue_read_queue(bucket_t *bucket, read_ctx_t *rctx)
{
    enqueue_work(bucket->read_queue, (void*)rctx);
}

/* ================ bucket_enqueue_delete_queue() ================ */
void bucket_enqueue_delete_queue(bucket_t *bucket, delete_ctx_t *dctx)
{
    enqueue_work(bucket->read_queue, (void*)dctx);
}

