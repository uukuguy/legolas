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

#define NUM_ACTORS 10
#define WORKER_READY "\001"
#define WORKER_ACK "\002"

#define ACTOR_READY "ACTOR READY"
#define ACTOR_OVER "ACTOR OVER"

extern const char *edbroker_backend_endpoint;

typedef struct worker_t{
    int id;
    vnode_t *vnode;

    zactor_t *actor;
    zloop_t *loop;
} worker_t;

worker_t *worker_new(int worker_id, int storage_type)
{
    worker_t *worker = (worker_t*)malloc(sizeof(worker_t));
    memset(worker, 0, sizeof(worker_t));

    worker->id = worker_id;

    const char *storage_dir = "./data/storage";
    if ( mkdir_if_not_exist(storage_dir) != 0 ){
        printf("mkdir %s failed.\n", storage_dir);
        abort();
    }
    worker->vnode = vnode_new(storage_dir, worker_id, storage_type, NULL);

    return worker;
}

void worker_free(worker_t *worker)
{
    vnode_free(worker->vnode);
    free(worker);
}

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

void worker_thread_main(zsock_t *pipe, void *user_data)
{
    worker_t *worker = (worker_t*)user_data;
    int id = worker->id;

    printf("==-== Worker %d Ready.\n", id);

    zsock_signal(pipe, 0);

    zstr_send(pipe, ACTOR_READY);

    zsock_t *sock_worker = zsock_new_req(edbroker_backend_endpoint);
    zframe_t *frame_ready = zframe_new(WORKER_READY, 1);
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
        zframe_t *frame_ACK = zframe_new(WORKER_ACK, 1);
        zmsg_add(rsp, frame_ACK);
        zmsg_send(&rsp, sock_worker);
    }

    zstr_send(pipe, ACTOR_OVER);

    zsock_destroy(&sock_worker);

    printf("==-== Worker %d Exit.\n", id);

}

static int over_actors = 0;

int handle_read_on_worker_pipe(zloop_t *loop, zsock_t *pipe, void *user_data)
{
    worker_t *worker = (worker_t*)user_data;

    if ( over_actors >= NUM_ACTORS ){
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
        printf("==-== Actor %d over! (%d/%d)\n", worker->id, over_actors, NUM_ACTORS);
    }

    zmsg_destroy(&msg);

    return 0;
}

int run_worker(void)
{
    zloop_t *loop = zloop_new();
    zloop_set_verbose(loop, 1);

    zactor_t *actors[NUM_ACTORS];
    worker_t *workers[NUM_ACTORS];
    for ( int i = 0 ; i < NUM_ACTORS ; i++ ){
        worker_t *worker = worker_new(i, STORAGE_LOGFILE);
        worker->loop = loop;
        zactor_t *actor = zactor_new(worker_thread_main, worker);
        worker->actor = actor;

        actors[i] = actor;
        workers[i] = worker;
    }

    for ( int i = 0 ; i < NUM_ACTORS ; i++ ){
        zactor_t *actor = actors[i];
        zloop_reader(loop, (zsock_t*)zactor_resolve(actor), handle_read_on_worker_pipe, workers[i]);
    }

    zloop_start(loop);

    for ( int i = 0 ; i < NUM_ACTORS ; i++ ){
        zactor_destroy(&actors[i]);
        worker_free(workers[i]);
    }

    zloop_destroy(&loop);

    return 0;
}

