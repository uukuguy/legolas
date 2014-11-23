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

#include "container.h"
#include "bucket.h"
#include "channel.h"

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
    vnode_t *vnode = bucket->vnode;
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
    vnode_t *vnode = bucket->vnode;
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
    container_t *container = bucket->container;

    trace_log("Bucket %d in worker(%d) Ready.", bucket->id, container->id);

    ZPIPE_ACTOR_THREAD_BEGIN(pipe);
    {

        uint32_t total_channels = 2;
        ZPIPE_NEW_BEGIN(bucket, total_channels);

        channel_t *channel = channel_new(i, bucket);

        ZPIPE_NEW_END(bucket, channel);

        ZPIPE_LOOP(bucket);

    }

    ZPIPE_ACTOR_THREAD_END(pipe);

    trace_log("Bucket(%d) Container(%d) Exit.", bucket->id, container->id);
}

/* ================ bucket_new() ================ */
bucket_t *bucket_new(int bucket_id, container_t *container, int storage_type, const char *broker_endpoint)
{
    bucket_t *bucket = (bucket_t*)malloc(sizeof(bucket_t));
    memset(bucket, 0, sizeof(bucket_t));

    bucket->container = container;
    bucket->id = bucket_id;
    bucket->broker_endpoint = broker_endpoint;

    /* -------- bucket->vnode -------- */
    bucket->vnode = vnode_new(container->data_dir, bucket_id, storage_type, NULL);

    bucket->heartbeat_at = zclock_time() + HEARTBEAT_INTERVAL;

    /* -------- bucket->actor -------- */
    ZPIPE_ACTOR_NEW(bucket, bucket_thread_main);

    return bucket;
}

/* ================ bucket_free() ================ */
void bucket_free(bucket_t *bucket)
{
    ZPIPE_FREE(bucket, channel_free);

    if ( bucket->vnode != NULL ){
        vnode_free(bucket->vnode);
        bucket->vnode = NULL;
    }

    ZPIPE_ACTOR_FREE(bucket);

    free(bucket);
}

