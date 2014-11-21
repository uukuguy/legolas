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

/* ================ bucket_connect_to_broker() ================ */
zsock_t *bucket_connect_to_broker(bucket_t *bucket)
{
    zsock_t *broker_sock = zsock_new_dealer(bucket->broker_endpoint);

    if ( broker_sock != NULL ){
        zmsg_t *msg_worker_ready = create_status_message(MSG_STATUS_WORKER_READY);
        zmsg_addmem(msg_worker_ready, &bucket->container->id, sizeof(uint32_t));
        zmsg_addmem(msg_worker_ready, &bucket->id, sizeof(bucket->id));
        zmsg_send(&msg_worker_ready, broker_sock);
    } else {
        warning_log("Bucket(%d) connect to broker failed. endpoint:%s", bucket->id, bucket->broker_endpoint);
    }

    return broker_sock;
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

/* ================ channel_connect_to_broker() ================ */
zsock_t *channel_connect_to_broker(channel_t *channel)
{
    bucket_t *bucket = channel->bucket;
    container_t *container = bucket->container;

    zsock_t *broker_sock = zsock_new_dealer(channel->broker_endpoint);

    if ( broker_sock != NULL ){
        zmsg_t *msg_worker_ready = create_status_message(MSG_STATUS_WORKER_READY);
        zmsg_addmem(msg_worker_ready, &channel->bucket->container->id, sizeof(uint32_t));
        zmsg_addmem(msg_worker_ready, &channel->bucket->id, sizeof(uint32_t));
        zmsg_addmem(msg_worker_ready, &channel->id, sizeof(uint32_t));
        zmsg_send(&msg_worker_ready, broker_sock);
    } else {
        warning_log("Channel(%d) int bucket(%d) container(%d) connect to broker failed. endpoint:%s", channel->id, bucket->id, container->id, channel->broker_endpoint);
    }

    return broker_sock;
}

/* ================ channel_thread_main() ================ */
void channel_thread_main(zsock_t *pipe, void *user_data)
{
    channel_t *channel = (channel_t*)user_data;
    bucket_t *bucket = channel->bucket;
    container_t *container = bucket->container;

    trace_log("Channel %d in bucket(%d) container(%d) Ready.", channel->id, bucket->id, container->id);

    zsock_signal(pipe, 0);
    message_send_status(pipe, MSG_STATUS_ACTOR_READY);

    zsock_t *broker_sock = channel_connect_to_broker(channel);
    if ( broker_sock == NULL ){
    }

    uint32_t interval = INTERVAL_INIT;
    uint32_t liveness = HEARTBEAT_LIVENESS * 2;

    zpoller_t *poller = zpoller_new(broker_sock, NULL);
    while ( true ){
        zsock_t *sock = zpoller_wait(poller, HEARTBEAT_INTERVAL / 2);

        if ( zclock_time() > bucket->heartbeat_at ){
            trace_log("--> Channel(%d) Bucket(%d) Container(%d) Send worker heartbeat.", channel->id, bucket->id, container->id);
            bucket->heartbeat_at = zclock_time() + HEARTBEAT_INTERVAL;

            message_send_heartbeat(broker_sock, MSG_HEARTBEAT_WORKER);
        }

        if ( sock != NULL ){
            zmsg_t *msg = zmsg_recv(sock);
            if ( msg == NULL ){
                break;
            }
            /*zmsg_print(msg);*/

            if ( message_check_heartbeat(msg, MSG_HEARTBEAT_BROKER) == 0 ){
            trace_log("--> Channel(%d) Bucket(%d) Container(%d) Receive broker heartbeat.", channel->id, bucket->id, container->id);
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

                warning_log("Channel(%d) Bucket(%d) Container(%d) timeout. Try reconnect...", channel->id, bucket->id, container->id);
                zsock_destroy(&broker_sock);
                broker_sock = channel_connect_to_broker(channel);

                liveness = HEARTBEAT_LIVENESS;
            }
        }

    }
    zpoller_destroy(&poller);

    message_send_status(pipe, MSG_STATUS_ACTOR_OVER);

    zsock_destroy(&broker_sock);

    trace_log("Channel(%d) Bucket(%d) Container(%d) Exit.", channel->id, channel->bucket->id, channel->bucket->container->id);

}

/* ================ channel_new() ================ */
channel_t *channel_new(uint32_t channel_id, bucket_t *bucket)
{
    channel_t *channel = (channel_t*)malloc(sizeof(channel_t));
    memset(channel, 0, sizeof(channel_t));

    channel->bucket = bucket;
    channel->id = channel_id;
    channel->broker_endpoint = bucket->broker_endpoint;
    channel->heartbeat_at = zclock_time() + HEARTBEAT_INTERVAL;

    /* --------channel->actor -------- */
    channel->actor = zactor_new(channel_thread_main, channel);

    return channel;
}

/* ================ channel_free() ================ */
void channel_free(channel_t *channel)
{
    if ( channel->broker_sock != NULL ){
        zsock_destroy(&channel->broker_sock);
        channel->broker_sock = NULL;
    }

    free(channel);
}

/* ================ handle_pullin_on_channel_pipe() ================ */
int handle_pullin_on_channel_pipe(zloop_t *loop, zsock_t *pipe, void *user_data)
{
    channel_t *channel = (channel_t*)user_data;
    bucket_t *bucket = channel->bucket;
    container_t *container = bucket->container;

    if ( bucket->total_over_channels >= bucket->total_channels ){
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
        bucket->total_over_channels++;
        notice_log("Channel(%d) Bucket(%d) Container(%d) over! (%d/%d)", channel->id, bucket->id, container->id, bucket->total_over_channels, bucket->total_channels);
    }

    zmsg_destroy(&msg);

    return 0;
}
/* ================ new_bucket_thread_main() ================ */
void new_bucket_thread_main(zsock_t *pipe, void *user_data)
{
    bucket_t *bucket = (bucket_t*)user_data;
    container_t *container = bucket->container;

    trace_log("Bucket %d in worker(%d) Ready.", bucket->id, container->id);

    zsock_signal(pipe, 0);
    message_send_status(pipe, MSG_STATUS_ACTOR_READY);

    int verbose = container->verbose;
    zloop_t *loop = zloop_new();
    zloop_set_verbose(loop, verbose);

    uint32_t total_channels = bucket->total_channels;

    for ( int i = 0 ; i < total_channels ; i++ ){
        zactor_t *actor = bucket->channels[i]->actor;
        zloop_reader(loop, (zsock_t*)zactor_resolve(actor), handle_pullin_on_channel_pipe, bucket->channels[i]);
    }

    zloop_start(loop);

    zloop_destroy(&loop);

    trace_log("Bucket(%d) Container(%d) Exit.", bucket->id, container->id);
}

/* ================ bucket_thread_main() ================ */
void bucket_thread_main(zsock_t *pipe, void *user_data)
{
    bucket_t *bucket = (bucket_t*)user_data;

    trace_log("Bucket %d in worker(%d) Ready.", bucket->id, bucket->container->id);

    zsock_signal(pipe, 0);
    message_send_status(pipe, MSG_STATUS_ACTOR_READY);

    zsock_t *broker_sock = bucket_connect_to_broker(bucket);
    if ( broker_sock == NULL ){
        warning_log("Bucket(%d) onnect to broker failed. endpoint:%s", bucket->id, bucket->broker_endpoint);
    }

    uint32_t interval = INTERVAL_INIT;
    uint32_t liveness = HEARTBEAT_LIVENESS * 2;

    zpoller_t *poller = zpoller_new(broker_sock, NULL);
    while ( true ){
        zsock_t *sock = zpoller_wait(poller, HEARTBEAT_INTERVAL / 2);

        if ( zclock_time() > bucket->heartbeat_at ){
            trace_log("--> Bucket(%d) Send worker heartbeat.", bucket->id);
            bucket->heartbeat_at = zclock_time() + HEARTBEAT_INTERVAL;

            message_send_heartbeat(broker_sock, MSG_HEARTBEAT_WORKER);
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
                zsock_destroy(&broker_sock);
                broker_sock = bucket_connect_to_broker(bucket);

                liveness = HEARTBEAT_LIVENESS;
            }
        }

    }
    zpoller_destroy(&poller);

    message_send_status(pipe, MSG_STATUS_ACTOR_OVER);

    zsock_destroy(&broker_sock);

    trace_log("Bucket(%d) Exit.", bucket->id);

}

// in file bucket_queue.c. 
void write_queue_callback(work_queue_t *wq);
void read_queue_callback(work_queue_t *wq);
void delete_queue_callback(work_queue_t *wq);
/* ================ bucket_new() ================ */
bucket_t *bucket_new(int bucket_id, container_t *container, int storage_type, const char *broker_endpoint)
{
    bucket_t *bucket = (bucket_t*)malloc(sizeof(bucket_t));
    memset(bucket, 0, sizeof(bucket_t));

    bucket->container = container;
    bucket->id = bucket_id;
    bucket->container = container;
    bucket->broker_endpoint = broker_endpoint;

    /* -------- bucket->vnode -------- */
    bucket->vnode = vnode_new(container->data_dir, bucket_id, storage_type, NULL);

    bucket->heartbeat_at = zclock_time() + HEARTBEAT_INTERVAL;

    /* -------- bucket->xxx_queue -------- */
    bucket->write_queue = init_work_queue(write_queue_callback, 0);
    bucket->read_queue = init_work_queue(read_queue_callback, 0);
    bucket->delete_queue = init_work_queue(delete_queue_callback, 0);

    /* -------- bucket->channels -------- */
    bucket->total_channels = 2;
    bucket->channels = (channel_t**)malloc(sizeof(channel_t*) * bucket->total_channels);
    for ( int i = 0 ; i < bucket->total_channels ; i++ ){
        bucket->channels[i] = channel_new(i, bucket);
    }

    /* -------- bucket->actor -------- */
    bucket->actor = zactor_new(new_bucket_thread_main, bucket);
    /*bucket->actor = zactor_new(bucket_thread_main, bucket);*/

    return bucket;
}

/* ================ bucket_free() ================ */
void bucket_free(bucket_t *bucket)
{
    zactor_destroy(&bucket->actor);
    bucket->actor = NULL;

    if ( bucket->channels != NULL ){
        for ( int i = 0 ; i < bucket->total_channels ; i++ ){
            if ( bucket->channels[i] != NULL ){
                channel_free(bucket->channels[i]);
                bucket->channels[i] = NULL;
            }
        }
        free(bucket->channels);
        bucket->channels = NULL;
    }

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

