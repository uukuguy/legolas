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

#define HEARTBEAT_INTERVAL 1000
#define HEARTBEAT_LIVENESS 5
#define INTERVAL_INIT 1000
#define INTERVAL_MAX 32000

/* -------- struct worker_t -------- */
typedef struct worker_t{
    int id;
    vnode_t *vnode;

    const char *edbroker_backend_endpoint;
    zactor_t *actor;

    int64_t heartbeat_at;

    zloop_t *loop;
} worker_t;

zmsg_t *worker_del_data(worker_t *worker, zmsg_t *msg)
{
    zmsg_t *sendback_msg = NULL;

    if ( sendback_msg == NULL ){
        sendback_msg = create_status_message(MSG_STATUS_WORKER_ERROR);
    }

    return sendback_msg;
}


zmsg_t *worker_get_data(worker_t *worker, zmsg_t *msg)
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
                char *slice_key = (char *)&key_md5; 
                uint32_t slice_key_len = sizeof(key_md5);

                char *slice_data = NULL;
                uint32_t slice_data_size = 0;
                int rc = kvdb_get(worker->vnode->kvdb, slice_key, slice_key_len, (void**)&slice_data, &slice_data_size);
                if ( rc == 0 ){
                    if ( slice_data != NULL ){
                        sendback_msg = create_key_data_message(key, slice_data, slice_data_size);
                    } else {
                        sendback_msg = create_status_message(MSG_STATUS_WORKER_NOTFOUND);
                    }
                }
                /*
                object_t *object = vnode_read_from_storage(worker->vnode, key_md5);
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
                    
                } else {
                    sendback_msg = create_status_message(MSG_STATUS_WORKER_NOTFOUND);
                } // object != NULL*/
            } // frame_key != NULL
        } // frame_action != NULL
    } // frame_msgtype != NULL

    if ( sendback_msg == NULL ){
        sendback_msg = create_status_message(MSG_STATUS_WORKER_ERROR);
    }

    return sendback_msg;
}

zmsg_t *worker_put_data(worker_t *worker, zmsg_t *msg)
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

                    md5_value_t key_md5;
                    md5(&key_md5, (uint8_t *)key, key_len);
                    char *slice_key = (char *)&key_md5; 
                    uint32_t slice_key_len = sizeof(key_md5);
                    int rc = kvdb_put(worker->vnode->kvdb, slice_key, slice_key_len, (void*)data, data_size);
                    if ( rc == 0 )
                    {
                    /*object_t *object = object_new(key, key_len);*/
                    /*object->object_size = data_size;*/
                    /*object_add_slice(object, data, data_size);*/
                    /*vnode_write_to_storage(worker->vnode, object);*/
                    /*object_free(object);*/

                    /*uint32_t default_slice_size = 1024 * 16;*/
                    /*uint32_t writed_size = 0;*/
                    /*int idx = 0;*/
                    /*while ( writed_size < data_size ){*/
                        /*char slice_key[NAME_MAX];*/
                        /*sprintf(slice_key, "%s-%d", key, idx);*/
                        /*uint32_t slice_size = writed_size + default_slice_size <= data_size ? default_slice_size : data_size - writed_size;*/

                        /*object_t *object = object_new(slice_key, strlen(slice_key));*/
                        /*object->object_size = slice_size;*/
                        /*object_add_slice(object, &data[writed_size], slice_size);*/
                        /*vnode_write_to_storage(worker->vnode, object);*/
                        /*object_free(object);*/

                        /*idx++;*/
                        /*writed_size += slice_size;*/
                    /*}*/

                    sendback_msg = create_status_message(MSG_STATUS_WORKER_ACK);
                    }
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
zmsg_t *worker_handle_message(worker_t *worker, zmsg_t *msg)
{
    /*zmsg_print(msg);*/

    zmsg_t *sendback_msg = NULL;

    if ( message_check_action(msg, MSG_ACTION_PUT) == 0 ){ 
        sendback_msg = worker_put_data(worker, msg);
    } else if (message_check_action(msg, MSG_ACTION_GET) == 0 ) {
        sendback_msg = worker_get_data(worker, msg);
    } else if (message_check_action(msg, MSG_ACTION_DEL) == 0 ) {
        sendback_msg = worker_del_data(worker, msg);
    }

    if ( sendback_msg == NULL ){
        sendback_msg = create_status_message(MSG_STATUS_WORKER_ERROR);
    }

    return sendback_msg;
}

/* ================ worker_new_sock() ================ */
zsock_t *worker_new_sock(worker_t *worker)
{
    zsock_t *sock_worker = zsock_new_dealer(worker->edbroker_backend_endpoint);

    if ( sock_worker != NULL ){
        message_send_status(sock_worker, MSG_STATUS_WORKER_READY);
    }

    return sock_worker;
}

/* ================ worker_thread_main_for_dealer() ================ */
void worker_thread_main(zsock_t *pipe, void *user_data)
{
    worker_t *worker = (worker_t*)user_data;
    int id = worker->id;

    trace_log("Worker %d Ready.", id);

    zsock_signal(pipe, 0);

    message_send_status(pipe, MSG_STATUS_ACTOR_READY);

    zsock_t *sock_worker = worker_new_sock(worker);

    uint32_t interval = INTERVAL_INIT;
    uint32_t liveness = HEARTBEAT_LIVENESS * 2;

    while ( true ){
        zpoller_t *poller = zpoller_new(sock_worker, NULL);
        zsock_t *sock = zpoller_wait(poller, HEARTBEAT_INTERVAL / 2);

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
                zframe_t *identity = zmsg_unwrap(msg);
                zmsg_t *sendback_msg = worker_handle_message(worker, msg);
                zmsg_destroy(&msg);

                assert(sendback_msg != NULL);
                /*zmsg_t *sendback_msg = zmsg_new();*/
                zmsg_wrap(sendback_msg, identity);

                /*message_add_status(sendback_msg, rc == 0 ? MSG_STATUS_WORKER_ACK : MSG_STATUS_WORKER_ERROR);*/

                zmsg_send(&sendback_msg, sock);
            }
        } else {
            if ( --liveness == 0 ){
                /*zclock_sleep(interval);*/
                if ( interval < INTERVAL_MAX ){
                    interval *= 2;
                }

                warning_log("Worker %d timeout. Try reconnect...", worker->id);
                zsock_destroy(&sock_worker);
                sock_worker = worker_new_sock(worker);

                liveness = HEARTBEAT_LIVENESS;
            }
        }

        if ( zclock_time() > worker->heartbeat_at ){
            trace_log("--> Send worker heartbeat.");
            worker->heartbeat_at = zclock_time() + HEARTBEAT_INTERVAL;

            message_send_heartbeat(sock_worker, MSG_HEARTBEAT_WORKER);
        }

        zpoller_destroy(&poller);
    }

    message_send_status(pipe, MSG_STATUS_ACTOR_OVER);

    zsock_destroy(&sock_worker);

    trace_log("Worker %d Exit.", id);

}

/* ================ worker_thread_main_for_req() ================ */
/*void worker_thread_main_for_req(zsock_t *pipe, void *user_data)*/
/*{*/
    /*worker_t *worker = (worker_t*)user_data;*/
    /*int id = worker->id;*/

    /*trace_log("Worker %d Ready.", id);*/

    /*zsock_signal(pipe, 0);*/

    /*message_send_status(pipe, MSG_STATUS_ACTOR_READY);*/

    /*zsock_t *sock_worker = zsock_new_req(worker->edbroker_backend_endpoint);*/

    /*message_send_status(sock_worker, MSG_STATUS_WORKER_READY);*/

    /*while ( true ){*/
        /*zmsg_t *msg = zmsg_recv(sock_worker);*/
        /*if ( msg == NULL ){*/
            /*break;*/
        /*}*/

        /*zframe_t *identity = zmsg_unwrap(msg);*/
        /*worker_handle_message(worker, msg);*/
        /*zmsg_destroy(&msg);*/

        /*zmsg_t *sendback_msg = zmsg_new();*/
        /*zmsg_wrap(sendback_msg, identity);*/

        /*message_add_status(sendback_msg, MSG_STATUS_WORKER_ACK);*/

        /*zmsg_send(&sendback_msg, sock_worker);*/
    /*}*/

    /*message_send_status(pipe, MSG_STATUS_ACTOR_OVER);*/

    /*zsock_destroy(&sock_worker);*/

    /*trace_log("Worker %d Exit.", id);*/

/*}*/

/* ================ worker_new() ================ */
worker_t *worker_new(int worker_id, int storage_type, const char *edbroker_backend_endpoint)
{
    worker_t *worker = (worker_t*)malloc(sizeof(worker_t));
    memset(worker, 0, sizeof(worker_t));

    worker->id = worker_id;
    worker->edbroker_backend_endpoint = edbroker_backend_endpoint;

    const char *storage_dir = "./data/storage";
    if ( mkdir_if_not_exist(storage_dir) != 0 ){
        error_log("mkdir %s failed.", storage_dir);
        abort();
    }
    worker->vnode = vnode_new(storage_dir, worker_id, storage_type, NULL);
    worker->heartbeat_at = zclock_time() + HEARTBEAT_INTERVAL;

    worker->actor = zactor_new(worker_thread_main, worker);

    return worker;
}

/* ================ worker_free() ================ */
void worker_free(worker_t *worker)
{
    zactor_destroy(&worker->actor);
    worker->actor = NULL;
    vnode_free(worker->vnode);
    worker->vnode = NULL;
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
int run_worker(const char *endpoint, int total_threads, int storage_type, int verbose)
{
    info_log("run_worker() with %d threads connect to %s. Storage Type(%d):%s", total_threads, endpoint, storage_type, get_storage_type_name(storage_type));

    total_actors = total_threads;

    zloop_t *loop = zloop_new();
    zloop_set_verbose(loop, verbose);

    worker_t **workers = (worker_t**)malloc(sizeof(worker_t*) * total_actors);
    for ( int i = 0 ; i < total_actors ; i++ ){
        worker_t *worker = worker_new(i, storage_type, endpoint);
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

