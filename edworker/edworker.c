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


#define MSG_WORKER_ERROR "\0xFF"
#define MSG_WORKER_READY "\001"
#define MSG_WORKER_HEARTBEAT "\002"
#define MSG_WORKER_ACK "\003"

#define ACTOR_READY "ACTOR READY"
#define ACTOR_OVER "ACTOR OVER"

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

/* ================ worker_handle_message() ================ */
int worker_handle_message(worker_t *worker, zmsg_t *msg)
{
    /*zmsg_print(msg);*/

    zframe_t *frame = zmsg_first(msg);
    const char *data = (const char *)zframe_data(frame);
    uint32_t data_size = zframe_size(frame);

    object_t *object = object_new("key", 3);

    object_add_slice(object, data, data_size);

    object->object_size = data_size;

    vnode_write_to_storage(worker->vnode, object);

    object_free(object);

    return 0;
}

/* ================ worker_new_sock() ================ */
zsock_t *worker_new_sock(worker_t *worker)
{
    zsock_t *sock_worker = zsock_new_dealer(worker->edbroker_backend_endpoint);

    zframe_t *frame_ready = zframe_new(MSG_WORKER_READY, strlen(MSG_WORKER_READY));
    zframe_send(&frame_ready, sock_worker, 0);

    return sock_worker;
}

/* ================ worker_thread_main_for_dealer() ================ */
void worker_thread_main(zsock_t *pipe, void *user_data)
{
    worker_t *worker = (worker_t*)user_data;
    int id = worker->id;

    trace_log("Worker %d Ready.", id);

    zsock_signal(pipe, 0);

    zstr_send(pipe, ACTOR_READY);

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

            if ( zmsg_size(msg) == strlen(MSG_WORKER_HEARTBEAT) ){
                trace_log("Receive broker heartbeat.");
                liveness = HEARTBEAT_LIVENESS;
                zmsg_destroy(&msg);
            } else {
                zframe_t *identity = zmsg_unwrap(msg);
                worker_handle_message(worker, msg);
                zmsg_destroy(&msg);

                zmsg_t *rsp = zmsg_new();
                zmsg_wrap(rsp, identity);
                zframe_t *frame_ACK = zframe_new(MSG_WORKER_ACK, strlen(MSG_WORKER_ACK));
                zmsg_add(rsp, frame_ACK);
                zmsg_send(&rsp, sock);
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
            trace_log("Send broker heartbeat.");
            worker->heartbeat_at = zclock_time() + HEARTBEAT_INTERVAL;
            zframe_t *frame = zframe_new(MSG_WORKER_HEARTBEAT, strlen(MSG_WORKER_HEARTBEAT));
            zframe_send(&frame, sock_worker, 0);
        }

        zpoller_destroy(&poller);
    }

    zstr_send(pipe, ACTOR_OVER);

    zsock_destroy(&sock_worker);

    trace_log("Worker %d Exit.", id);

}

/* ================ worker_thread_main_for_req() ================ */
void worker_thread_main_for_req(zsock_t *pipe, void *user_data)
{
    worker_t *worker = (worker_t*)user_data;
    int id = worker->id;

    trace_log("Worker %d Ready.", id);

    zsock_signal(pipe, 0);

    zstr_send(pipe, ACTOR_READY);

    zsock_t *sock_worker = zsock_new_req(worker->edbroker_backend_endpoint);
    zframe_t *frame_ready = zframe_new(MSG_WORKER_READY, strlen(MSG_WORKER_READY));
    zframe_send(&frame_ready, sock_worker, 0);

    while ( true ){
        zmsg_t *msg = zmsg_recv(sock_worker);
        if ( msg == NULL ){
            break;
        }
        /*zmsg_print(msg);*/

        zframe_t *identity = zmsg_unwrap(msg);
        worker_handle_message(worker, msg);
        zmsg_destroy(&msg);

        zmsg_t *rsp = zmsg_new();
        zmsg_wrap(rsp, identity);
        zframe_t *frame_ACK = zframe_new(MSG_WORKER_ACK, strlen(MSG_WORKER_ACK));
        zmsg_add(rsp, frame_ACK);
        zmsg_send(&rsp, sock_worker);
    }

    zstr_send(pipe, ACTOR_OVER);

    zsock_destroy(&sock_worker);

    trace_log("Worker %d Exit.", id);

}

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

    char *actor_rsp = zmsg_popstr(msg);
    if ( strcmp(actor_rsp, ACTOR_OVER) == 0 ){
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

