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
#include "filesystem.h"
#include "vnode.h"
#include "logger.h"
#include "md5.h"
#include "adlist.h"
#include "everdata.h"
#include "work.h"

#define HEARTBEAT_INTERVAL 1000
#define HEARTBEAT_LIVENESS 5
#define INTERVAL_INIT 1000
#define INTERVAL_MAX 32000

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

/* -------- struct worker_t -------- */
typedef struct worker_t{
    uint32_t id;
    /*vnode_t *vnode;*/
    vnode_t **vnodes;
    int total_writers;
    work_queue_t *write_queue;
    work_queue_t *read_queue;
    work_queue_t *delete_queue;

    const char *edbroker_backend_endpoint;
    zactor_t *actor;

    int64_t heartbeat_at;

    zloop_t *loop;
} worker_t;

void worker_enqueue_write_queue(worker_t *worker, write_ctx_t *wctx)
{
    enqueue_work(worker->write_queue, (void*)wctx);
}

void worker_enqueue_read_queue(worker_t *worker, read_ctx_t *rctx)
{
    enqueue_work(worker->read_queue, (void*)rctx);
}

void worker_enqueue_delete_queue(worker_t *worker, delete_ctx_t *dctx)
{
    enqueue_work(worker->read_queue, (void*)dctx);
}

vnode_t *worker_get_vnode_by_key(worker_t *worker, uint32_t h2)
{
    uint32_t d1 = h2 % worker->total_writers;
    return worker->vnodes[d1];
}

zmsg_t *worker_del_data(worker_t *worker, zsock_t *sock, zframe_t *identity, zmsg_t *msg)
{
    zmsg_t *sendback_msg = NULL;

    if ( sendback_msg == NULL ){
        sendback_msg = create_status_message(MSG_STATUS_WORKER_ERROR);

        zmsg_wrap(sendback_msg, identity);
        zmsg_send(&sendback_msg, sock);
    }

    return sendback_msg;
}


zmsg_t *worker_get_data(worker_t *worker, zsock_t *sock, zframe_t *identity, zmsg_t *msg)
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

                /*vnode_t *vnode = woker->vnode;*/
                vnode_t *vnode = worker_get_vnode_by_key(worker, key_md5.h2);

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

zmsg_t *worker_put_data(worker_t *worker, zsock_t *sock, zframe_t *identity, zmsg_t *msg)
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

                    /*vnode_t *vnode = woker->vnode;*/
                    vnode_t *vnode = worker_get_vnode_by_key(worker, object->key_md5.h2);

                    object->object_size = data_size;
                    object_add_slice(object, data, data_size);

                    /* FiXME */
                    /*write_ctx_t *writer = writer_new(sock, identity, vnode, object);*/
                    /*worker_enqueue_write_queue(worker, writer);*/
                    /*return NULL;*/

                    vnode_write_to_storage(vnode, object);
                    object_free(object);

                    sendback_msg = create_status_message(MSG_STATUS_WORKER_ACK);

                    /*}*/
                }
            }
        }
    }
    if ( sendback_msg == NULL ){
        sendback_msg = create_status_message(MSG_STATUS_WORKER_ERROR);
    }

    return sendback_msg;
}

/* ================ worker_handle_message() ================ */
int worker_handle_message(worker_t *worker, zsock_t *sock, zmsg_t *msg)
{
    /*zmsg_print(msg);*/

    zframe_t *identity = zmsg_unwrap(msg);

    zmsg_t *sendback_msg = NULL;

    if ( message_check_action(msg, MSG_ACTION_PUT) == 0 ){ 
        sendback_msg = worker_put_data(worker, sock, identity, msg);
    } else if (message_check_action(msg, MSG_ACTION_GET) == 0 ) {
        sendback_msg = worker_get_data(worker, sock, identity, msg);
    } else if (message_check_action(msg, MSG_ACTION_DEL) == 0 ) {
        sendback_msg = worker_del_data(worker, sock, identity, msg);
    }

    zmsg_destroy(&msg);

    if (sendback_msg != NULL) {
        zmsg_wrap(sendback_msg, identity);
        zmsg_send(&sendback_msg, sock);
    }

    return 0;
}

/* ================ worker_connect_to_broker() ================ */
zsock_t *worker_connect_to_broker(worker_t *worker)
{
    zsock_t *sock_broker = zsock_new_dealer(worker->edbroker_backend_endpoint);

    if ( sock_broker != NULL ){
        /*message_send_status(sock_broker, MSG_STATUS_WORKER_READY);*/
        zmsg_t *msg_worker_ready = create_status_message(MSG_STATUS_WORKER_READY);
        zmsg_addmem(msg_worker_ready, &worker->id, sizeof(worker->id));
        zmsg_send(&msg_worker_ready, sock_broker);
    }

    return sock_broker;
}

/* ================ worker_thread_main_for_dealer() ================ */
void worker_thread_main(zsock_t *pipe, void *user_data)
{
    worker_t *worker = (worker_t*)user_data;

    trace_log("Worker %d Ready.", worker->id);

    zsock_signal(pipe, 0);

    message_send_status(pipe, MSG_STATUS_ACTOR_READY);

    zsock_t *sock_broker = worker_connect_to_broker(worker);

    uint32_t interval = INTERVAL_INIT;
    uint32_t liveness = HEARTBEAT_LIVENESS * 2;

    zpoller_t *poller = zpoller_new(sock_broker, NULL);
    while ( true ){
        zsock_t *sock = zpoller_wait(poller, HEARTBEAT_INTERVAL / 2);

        if ( zclock_time() > worker->heartbeat_at ){
            trace_log("--> Send worker heartbeat.");
            worker->heartbeat_at = zclock_time() + HEARTBEAT_INTERVAL;

            message_send_heartbeat(sock_broker, MSG_HEARTBEAT_WORKER);
        }

        if ( sock != NULL ){
            zmsg_t *msg = zmsg_recv(sock);
            if ( msg == NULL ){
                break;
            }
            /*zmsg_print(msg);*/

            if ( message_check_heartbeat(msg, MSG_HEARTBEAT_BROKER) == 0 ){
                trace_log("<-- Receive broker heartbeat.");
                liveness = HEARTBEAT_LIVENESS;
                zmsg_destroy(&msg);
            } else {
                worker_handle_message(worker, sock, msg);
            }
        } else {
            if ( --liveness == 0 ){
                /*zclock_sleep(interval);*/
                if ( interval < INTERVAL_MAX ){
                    interval *= 2;
                }

                warning_log("Worker %d timeout. Try reconnect...", worker->id);
                zsock_destroy(&sock_broker);
                sock_broker = worker_connect_to_broker(worker);

                liveness = HEARTBEAT_LIVENESS;
            }
        }

    }
    zpoller_destroy(&poller);

    message_send_status(pipe, MSG_STATUS_ACTOR_OVER);

    zsock_destroy(&sock_broker);

    trace_log("Worker %d Exit.", worker->id);

}

/* ================ worker_new() ================ */
worker_t *worker_new(int worker_id, int total_writers, int storage_type, const char *edbroker_backend_endpoint)
{
    worker_t *worker = (worker_t*)malloc(sizeof(worker_t));
    memset(worker, 0, sizeof(worker_t));

    worker->id = worker_id;
    worker->total_writers = total_writers;
    worker->edbroker_backend_endpoint = edbroker_backend_endpoint;

    const char *storage_dir = "./data/storage";
    if ( mkdir_if_not_exist(storage_dir) != 0 ){
        error_log("mkdir %s failed.", storage_dir);
        abort();
    }

    /*worker->vnode = vnode_new(storage_dir, worker_id, storage_type, NULL);*/
    worker->vnodes = (vnode_t**)malloc(sizeof(vnode_t*) * total_writers);
    char worker_dir[NAME_MAX];
    sprintf(worker_dir, "%s/%04d", storage_dir, worker_id);
    mkdir_if_not_exist(worker_dir);
    for ( int i = 0 ; i < total_writers ; i++ ){
        worker->vnodes[i] = vnode_new(worker_dir, i, storage_type, NULL);
    }

    worker->heartbeat_at = zclock_time() + HEARTBEAT_INTERVAL;

    worker->write_queue = init_work_queue(write_queue_callback, 0);
    worker->read_queue = init_work_queue(read_queue_callback, 0);
    worker->delete_queue = init_work_queue(delete_queue_callback, 0);

    worker->actor = zactor_new(worker_thread_main, worker);

    return worker;
}

/* ================ worker_free() ================ */
void worker_free(worker_t *worker)
{
    zactor_destroy(&worker->actor);
    worker->actor = NULL;

    if ( worker->write_queue != NULL ){
        exit_work_queue(worker->write_queue);
        free(worker->write_queue);
        worker->write_queue = NULL;
    }

    if ( worker->read_queue != NULL ){
        exit_work_queue(worker->read_queue);
        free(worker->read_queue);
        worker->read_queue = NULL;
    }

    if ( worker->delete_queue != NULL ){
        exit_work_queue(worker->delete_queue);
        free(worker->delete_queue);
        worker->delete_queue = NULL;
    }

    for ( int i = 0 ; i < worker->total_writers ; i++ ){
        vnode_free(worker->vnodes[i]);
    }
    free(worker->vnodes);
    worker->vnodes = NULL;
    /*vnode_free(worker->vnode);*/
    /*worker->vnode = NULL;*/

    free(worker);
}

static int over_actors = 0;
static uint32_t total_actors = 0;

/* ================ handle_pullin_on_worker_pipe() ================ */
int handle_pullin_on_worker_pipe(zloop_t *loop, zsock_t *pipe, void *user_data)
{
    worker_t *worker = (worker_t*)user_data;

    if ( over_actors >= total_actors ){
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
        over_actors++;
        notice_log("Actor %d over! (%d/%d)", worker->id, over_actors, total_actors);
    }

    zmsg_destroy(&msg);

    return 0;
}

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

/* ================ run_worker() ================ */
int run_worker(const char *endpoint, int total_threads, int total_writers, int storage_type, int verbose)
{
    info_log("run_worker() with %d threads connect to %s. Storage Type(%d):%s", total_threads, endpoint, storage_type, get_storage_type_name(storage_type));

    total_actors = total_threads;

    zloop_t *loop = zloop_new();
    zloop_set_verbose(loop, verbose);

    worker_t **workers = (worker_t**)malloc(sizeof(worker_t*) * total_actors);
    for ( int i = 0 ; i < total_actors ; i++ ){
        worker_t *worker = worker_new(i, total_writers, storage_type, endpoint);
        worker->loop = loop;

        workers[i] = worker;
    }

    for ( int i = 0 ; i < total_actors ; i++ ){
        zactor_t *actor = workers[i]->actor;
        zloop_reader(loop, (zsock_t*)zactor_resolve(actor), handle_pullin_on_worker_pipe, workers[i]);
    }

    zloop_start(loop);

    for ( int i = 0 ; i < total_actors ; i++ ){
        worker_free(workers[i]);
    }
    free(workers);

    zloop_destroy(&loop);

    return 0;
}

