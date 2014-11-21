/**
 * @file   bucket.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-21 15:09:27
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

write_ctx_t *write_ctx_new(zsock_t *sock, zframe_t *identity, vnode_t *vnode, object_t *object)
{
    write_ctx_t *wctx = (write_ctx_t*)malloc(sizeof(write_ctx_t));

    wctx->sock = sock;
    wctx->identity = identity;
    wctx->vnode = vnode;
    wctx->object = object;

    return wctx;
}

void writer_ctx_free(write_ctx_t *wctx)
{
    free(wctx);
}

read_ctx_t *read_ctx_new(zsock_t *sock, zframe_t *identity)
{
    read_ctx_t *rctx = (read_ctx_t*)malloc(sizeof(read_ctx_t));

    rctx->sock = sock;
    rctx->identity = identity;

    return rctx;
}

void read_ctx_free(read_ctx_t *rctx)
{
    free(rctx);
}

delete_ctx_t *delete_ctx_new(zsock_t *sock, zframe_t *identity)
{
    delete_ctx_t *dctx = (delete_ctx_t*)malloc(sizeof(delete_ctx_t));

    dctx->sock = sock;
    dctx->identity = identity;

    return dctx;
}

void delete_ctx_free(delete_ctx_t *dctx)
{
    free(dctx);
}

void bucket_thread_main(zsock_t *pipe, void *user_data);
/* ================ bucket_new() ================ */
bucket_t *bucket_new(int bucket_id, container_t *container, int storage_type, const char *broker_endpoint)
{
    bucket_t *bucket = (bucket_t*)malloc(sizeof(bucket_t));
    memset(bucket, 0, sizeof(bucket_t));

    bucket->id = bucket_id;
    bucket->container = container;
    bucket->broker_endpoint = broker_endpoint;

    bucket->vnode = vnode_new(container->data_dir, bucket_id, storage_type, NULL);

    bucket->heartbeat_at = zclock_time() + HEARTBEAT_INTERVAL;

    bucket->write_queue = init_work_queue(write_queue_callback, 0);
    bucket->read_queue = init_work_queue(read_queue_callback, 0);
    bucket->delete_queue = init_work_queue(delete_queue_callback, 0);

    bucket->actor = zactor_new(bucket_thread_main, bucket);

    return bucket;
}

/* ================ bucket_free() ================ */
void bucket_free(bucket_t *bucket)
{
    zactor_destroy(&bucket->actor);
    bucket->actor = NULL;

    if ( bucket->write_queue != NULL ){
        exit_work_queue(bucket->write_queue);
        free(bucket->write_queue);
        bucket->write_queue = NULL;
    }

    if ( bucket->read_queue != NULL ){
        exit_work_queue(bucket->read_queue);
        free(bucket->read_queue);
        bucket->read_queue = NULL;
    }

    if ( bucket->delete_queue != NULL ){
        exit_work_queue(bucket->delete_queue);
        free(bucket->delete_queue);
        bucket->delete_queue = NULL;
    }

    if ( bucket->vnode != NULL ){
        vnode_free(bucket->vnode);
        bucket->vnode = NULL;
    }

    free(bucket);
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

/* ================ bucket_connect_to_broker() ================ */
zsock_t *bucket_connect_to_broker(bucket_t *bucket)
{
    zsock_t *sock_broker = zsock_new_dealer(bucket->broker_endpoint);

    if ( sock_broker != NULL ){
        zmsg_t *msg_worker_ready = create_status_message(MSG_STATUS_WORKER_READY);
        zmsg_addmem(msg_worker_ready, &bucket->container->id, sizeof(uint32_t));
        zmsg_addmem(msg_worker_ready, &bucket->id, sizeof(bucket->id));
        zmsg_send(&msg_worker_ready, sock_broker);
    }

    return sock_broker;
}

/* ================ bucket_del_data() ================ */
zmsg_t *bucket_del_data(bucket_t *bucket, zsock_t *sock, zframe_t *identity, zmsg_t *msg)
{
    zmsg_t *sendback_msg = NULL;

    if ( sendback_msg == NULL ){
        sendback_msg = create_status_message(MSG_STATUS_WORKER_ERROR);

        zmsg_wrap(sendback_msg, identity);
        zmsg_send(&sendback_msg, sock);
    }

    return sendback_msg;
}


/* ================ bucket_get_data() ================ */
zmsg_t *bucket_get_data(bucket_t *bucket, zsock_t *sock, zframe_t *identity, zmsg_t *msg)
{
    zmsg_t *sendback_msg = NULL;

    zframe_t *frame_msgtype = zmsg_first(msg);
    if ( frame_msgtype != NULL ){
        zframe_t *frame_action = zmsg_next(msg);
        if ( frame_action != NULL ){

            zframe_t *frame_key = zmsg_next(msg);
            if ( frame_key != NULL ){

                const char *key = (const char *)zframe_data(frame_key);
                uint32_t key_len = zframe_size(frame_key);

                md5_value_t key_md5;
                md5(&key_md5, (uint8_t *)key, key_len);

                vnode_t *vnode = bucket->vnode;

                object_t *object = vnode_read_from_storage(vnode, key_md5);
                if ( object != NULL ){
                    uint32_t object_size = object->object_size;
                    char *data = malloc(object_size);

                    listIter *iter = listGetIterator(object->slices, AL_START_HEAD);
                    listNode *node = NULL;
                    uint32_t pos = 0;
                    uint32_t data_size = 0;
                    while ( (node = listNext(iter)) != NULL ){
                        slice_t *slice = (slice_t*)node->value;
                        char *buf = slice->byteblock.buf;
                        uint32_t buf_size = slice->byteblock.size;
                        memcpy(&data[pos], buf, buf_size);
                        pos += buf_size;
                        data_size += buf_size;
                    };
                    listReleaseIterator(iter);
                    object_free(object);

                    assert (data_size == object_size);

                    sendback_msg = create_key_data_message(key, data, object_size);

                    free(data);
                    
                } else {
                    sendback_msg = create_status_message(MSG_STATUS_WORKER_NOTFOUND);
                } // object != NULL
            } // frame_key != NULL
        } // frame_action != NULL
    } // frame_msgtype != NULL

    if ( sendback_msg == NULL ){
        sendback_msg = create_status_message(MSG_STATUS_WORKER_ERROR);
    }

    return sendback_msg;
}

/* ================ bucket_put_data() ================ */
zmsg_t *bucket_put_data(bucket_t *bucket, zsock_t *sock, zframe_t *identity, zmsg_t *msg)
{ 
    zmsg_t *sendback_msg = NULL;

    UNUSED zframe_t *frame_msgtype = zmsg_first(msg);
    if ( frame_msgtype != NULL ){
        UNUSED zframe_t *frame_action = zmsg_next(msg);
        if ( frame_action != NULL ) {
            zframe_t *frame_key = zmsg_next(msg);
            if ( frame_key != NULL ) {
                const char *key = (const char *)zframe_data(frame_key);
                UNUSED uint32_t key_len = zframe_size(frame_key);

                zframe_t *frame = zmsg_next(msg);

                if ( frame != NULL ){
                    const char *data = (const char *)zframe_data(frame);
                    uint32_t data_size = zframe_size(frame);

                    object_t *object = object_new(key, key_len);
                    object->object_size = data_size;
                    object_add_slice(object, data, data_size);

                    vnode_t *vnode = bucket->vnode;

                    /* FiXME */
                    /*write_ctx_t *writer = write_ctx_new(sock, identity, vnode, object);*/
                    /*bucket_enqueue_write_queue(bucket, writer);*/
                    /*return NULL;*/

                    vnode_write_to_storage(vnode, object);
                    object_free(object);

                    sendback_msg = create_status_message(MSG_STATUS_WORKER_ACK);
                }
            }
        }
    }
    if ( sendback_msg == NULL ){
        sendback_msg = create_status_message(MSG_STATUS_WORKER_ERROR);
    }

    return sendback_msg;
}

/* ================ bucket_handle_message() ================ */
int bucket_handle_message(bucket_t *bucket, zsock_t *sock, zmsg_t *msg)
{
    /*zmsg_print(msg);*/

    zframe_t *identity = zmsg_unwrap(msg);

    zmsg_t *sendback_msg = NULL;

    if ( message_check_action(msg, MSG_ACTION_PUT) == 0 ){ 
        sendback_msg = bucket_put_data(bucket, sock, identity, msg);
    } else if (message_check_action(msg, MSG_ACTION_GET) == 0 ) {
        sendback_msg = bucket_get_data(bucket, sock, identity, msg);
    } else if (message_check_action(msg, MSG_ACTION_DEL) == 0 ) {
        sendback_msg = bucket_del_data(bucket, sock, identity, msg);
    }

    zmsg_destroy(&msg);

    if (sendback_msg != NULL) {
        zmsg_wrap(sendback_msg, identity);
        zmsg_send(&sendback_msg, sock);
    }

    return 0;
}

/* ================ bucket_thread_main() ================ */
void bucket_thread_main(zsock_t *pipe, void *user_data)
{
    bucket_t *bucket = (bucket_t*)user_data;

    trace_log("Bucket %d in worker(%d) Ready.", bucket->id, bucket->container->id);

    zsock_signal(pipe, 0);
    message_send_status(pipe, MSG_STATUS_ACTOR_READY);

    zsock_t *sock_broker = bucket_connect_to_broker(bucket);

    uint32_t interval = INTERVAL_INIT;
    uint32_t liveness = HEARTBEAT_LIVENESS * 2;

    zpoller_t *poller = zpoller_new(sock_broker, NULL);
    while ( true ){
        zsock_t *sock = zpoller_wait(poller, HEARTBEAT_INTERVAL / 2);

        if ( zclock_time() > bucket->heartbeat_at ){
            trace_log("--> Bucket(%d) Send worker heartbeat.", bucket->id);
            bucket->heartbeat_at = zclock_time() + HEARTBEAT_INTERVAL;

            message_send_heartbeat(sock_broker, MSG_HEARTBEAT_WORKER);
        }

        if ( sock != NULL ){
            zmsg_t *msg = zmsg_recv(sock);
            if ( msg == NULL ){
                break;
            }
            /*zmsg_print(msg);*/

            if ( message_check_heartbeat(msg, MSG_HEARTBEAT_BROKER) == 0 ){
                trace_log("<-- Bucket(%d) Receive broker heartbeat.", bucket->id);
                liveness = HEARTBEAT_LIVENESS;
                zmsg_destroy(&msg);
            } else {
                bucket_handle_message(bucket, sock, msg);
            }
        } else {
            if ( --liveness == 0 ){
                /*zclock_sleep(interval);*/
                if ( interval < INTERVAL_MAX ){
                    interval *= 2;
                }

                warning_log("Bucket(%d) timeout. Try reconnect...", bucket->id);
                zsock_destroy(&sock_broker);
                sock_broker = bucket_connect_to_broker(bucket);

                liveness = HEARTBEAT_LIVENESS;
            }
        }

    }
    zpoller_destroy(&poller);

    message_send_status(pipe, MSG_STATUS_ACTOR_OVER);

    zsock_destroy(&sock_broker);

    trace_log("Bucket(%d) Exit.", bucket->id);

}

