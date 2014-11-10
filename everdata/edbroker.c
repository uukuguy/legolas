/**
 * @file   edbroker.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-08 03:50:03
 * 
 * @brief  
 * 
 * 
 */

#include <czmq.h>
#include <pthread.h>
#include "common.h"
#include "logger.h"
#include "everdata.h"

#define HEARTBEAT_INTERVAL 1000
#define HEARTBEAT_LIVENESS 5

/* -------- struct worker_t -------- */
typedef struct worker_t {
    zframe_t *identity;
    char *id_string;
    int64_t expiry;
} worker_t;

worker_t *worker_new(zframe_t *identity)
{
    worker_t *worker = (worker_t*)malloc(sizeof(worker_t));
    memset(worker, 0, sizeof(worker_t));

    worker->identity = identity;
    worker->id_string = zframe_strhex(identity);
    worker->expiry = zclock_time() + HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS;

    return worker;
}

void worker_free(worker_t *worker)
{
    if ( worker->identity != NULL )
        zframe_destroy(&worker->identity);
    free(worker->id_string);
    free(worker);
}

/* -------- struct broker_t -------- */
typedef struct broker_t{
    zloop_t *loop;
    zsock_t *sock_local_frontend;
    zsock_t *sock_local_backend;
    int heartbeat_timer_id;
    int64_t heartbeat_at;

    zlist_t *workers;
    pthread_mutex_t workers_lock;
} broker_t;

broker_t *broker_new(void)
{
    broker_t *broker = (broker_t*)malloc(sizeof(broker_t));
    memset(broker, 0, sizeof(broker_t));

    pthread_mutex_init(&broker->workers_lock, NULL);

    broker->heartbeat_timer_id = -1;
    broker->heartbeat_at = zclock_time() + HEARTBEAT_INTERVAL;
    broker->workers = zlist_new();

    return broker;
}

void broker_lock_workers(broker_t *broker)
{
    /*pthread_mutex_lock(&broker->workers_lock);*/
}

void broker_unlock_workers(broker_t *broker)
{
    /*pthread_mutex_unlock(&broker->workers_lock);*/
}

void broker_free(broker_t *broker)
{
    zlist_t *workers = broker->workers;

    broker_lock_workers(broker);
    while ( zlist_size(workers) ){
        worker_t *w = (worker_t*)zlist_pop(workers);
        worker_free(w);
    }
    broker_unlock_workers(broker);

    pthread_mutex_destroy(&broker->workers_lock);
    zlist_destroy(&workers);
    free(broker);
}

uint32_t broker_get_available_workers(broker_t *broker)
{
    return zlist_size(broker->workers);
}

void broker_end_local_frontend(broker_t *broker)
{
    if ( broker->sock_local_frontend != NULL ){
        zloop_reader_end(broker->loop, broker->sock_local_frontend);
        broker->sock_local_frontend = NULL;
    }
}

void broker_end_local_backend(broker_t *broker)
{
    if ( broker->sock_local_backend != NULL ){
        zloop_reader_end(broker->loop, broker->sock_local_backend);
        broker->sock_local_backend = NULL;
    }
}

void broker_end_timer(broker_t *broker)
{
    if ( broker->heartbeat_timer_id != -1 ){
        zloop_timer_end(broker->loop, broker->heartbeat_timer_id);
        broker->heartbeat_timer_id = -1;
    }
}

void broker_end_loop(broker_t *broker)
{
    broker_end_local_frontend(broker);
    broker_end_local_backend(broker);
    broker_end_timer(broker);
}

/* ================ broker_set_worker_ready() ================ */
void broker_set_worker_ready(broker_t *broker, worker_t *worker)
{
    broker_lock_workers(broker);

    zlist_t *workers = broker->workers;

    worker_t *w = (worker_t*)zlist_first(workers);
    while ( w != NULL ){
        if ( strcmp(worker->id_string, w->id_string) == 0 ){
            zlist_remove(workers, w);
            worker_free(w);
            break;
        }
        w = (worker_t*)zlist_next(workers);
    };
    zlist_append(workers, worker);

    broker_unlock_workers(broker);
}

/* ================ broker_workers_pop_without_lock() ================ */
zframe_t *broker_workers_pop_without_lock(broker_t *broker)
{
    zframe_t *identity = NULL;

    zlist_t *workers = broker->workers;

    worker_t *worker = (worker_t*)zlist_pop(workers);
    if ( worker != NULL ){
        identity = worker->identity;
        worker->identity = NULL;
        worker_free(worker);
    }

    return identity;
}

/* ================ broker_workers_pop() ================ */
zframe_t *broker_workers_pop(broker_t *broker)
{
    zframe_t *identity = NULL;

    broker_lock_workers(broker);
    identity = broker_workers_pop_without_lock(broker);
    broker_unlock_workers(broker);

    return identity;
}
/* ================ broker_workers_purge() ================ */
void broker_workers_purge(broker_t *broker)
{
    zlist_t *workers = broker->workers;

    broker_lock_workers(broker);

    worker_t *w = (worker_t*)zlist_first(workers);
    while ( w != NULL ){
        int64_t now = zclock_time();
        int64_t expiry = w->expiry;
        if ( now < expiry ){
            break;
        }

        warning_log("Worker %s timeout. Remove from queue. now:%zu worker expiry:%zu(%d)", w->id_string, now, expiry, (int32_t)(expiry - now));

        zframe_t *frame = broker_workers_pop_without_lock(broker);
        if ( frame != NULL ){
            zframe_destroy(&frame);
        }

        w = (worker_t*)zlist_first(workers);
    };

    broker_unlock_workers(broker);
}

/* ================ handle_pullin_on_local_frontend() ================ */
int handle_pullin_on_local_frontend(zloop_t *loop, zsock_t *sock, void *user_data)
{
    broker_t *broker = (broker_t*)user_data;

    zmsg_t *msg = zmsg_recv(sock);
    if ( msg == NULL ){
        broker_end_loop(broker);
        return -1;
    }
    /*zmsg_print(msg);*/

    zframe_t *worker_identity = broker_workers_pop(broker);

    if ( worker_identity != NULL ){
        /* for req */
        /*zmsg_pushmem(msg, "", 0);*/

        zmsg_push(msg, worker_identity);

        zmsg_send(&msg, broker->sock_local_backend);
    } else {
        zmsg_t *sendback_msg = create_sendback_message(msg);
        message_add_status(sendback_msg, MSG_STATUS_WORKER_ERROR);

        zmsg_send(&sendback_msg, sock);
    }

    zmsg_destroy(&msg);

    return 0;
}

/* ================ handle_pullin_on_local_backend() ================ */
int handle_pullin_on_local_backend(zloop_t *loop, zsock_t *sock, void *user_data)
{
    broker_t *broker = (broker_t*)user_data;

    zmsg_t *msg = zmsg_recv(sock);
    if ( msg == NULL ){
        broker_end_loop(broker);
        return -1;
    }
    /*zmsg_print(msg);*/

    zframe_t *worker_identity = zmsg_unwrap(msg);
    assert(zframe_is(worker_identity));

    worker_t *worker = worker_new(worker_identity);
    broker_set_worker_ready(broker, worker);

    if ( message_check_heartbeat(msg, MSG_HEARTBEAT_WORKER) == 0 ){
        broker_lock_workers(broker);
        uint32_t available_workers = broker_get_available_workers(broker);
        int64_t now = zclock_time();
        int64_t expiry = 0;
        worker_t *w = (worker_t*)zlist_first(broker->workers);
        if ( w != NULL ){
            expiry = w->expiry;
        }
        broker_unlock_workers(broker);
        trace_log("<-- Receive worker heartbeat. Workers:%d. now:%zu first expiry:%zu(%d)", available_workers, now, expiry, (int32_t)(expiry - now));
        zmsg_destroy(&msg);

    } else if ( message_check_status(msg, MSG_STATUS_WORKER_READY) == 0 ) {
        broker_lock_workers(broker);
        uint32_t available_workers = broker_get_available_workers(broker);
        broker_unlock_workers(broker);
        notice_log("WORKER READY. Workers:%d", available_workers);
        zmsg_destroy(&msg);
    } 

    if ( msg != NULL ){
        /*zmsg_print(msg);*/
        zmsg_send(&msg, broker->sock_local_frontend);
    }

    return 0;
}

/* ================ handle_heartbeat_timer() ================ */
int handle_heartbeat_timer(zloop_t *loop, int timer_id, void *user_data)
{
    broker_t *broker = (broker_t*)user_data;
    assert(broker != NULL);
    zlist_t *workers = broker->workers;
    assert(workers != NULL);

    if ( zclock_time() >= broker->heartbeat_at ){

        broker->heartbeat_at = zclock_time() + HEARTBEAT_INTERVAL;

        broker_lock_workers(broker);
        worker_t *worker = (worker_t*)zlist_first(workers);
        while ( worker != NULL ){

            {
                uint32_t available_workers = broker_get_available_workers(broker);
                int64_t now = zclock_time();
                int64_t expiry = 0;
                expiry = worker->expiry;
                trace_log("--> Send broker heartbeat to worker %s. Workers:%d. now:%zu first expiry:%zu(%d)", worker->id_string, available_workers, now, expiry, (int32_t)(expiry - now));
            }

            zmsg_t *heartbeat_msg = zmsg_new();
            zmsg_push(heartbeat_msg, zframe_dup(worker->identity));
            message_add_heartbeat(heartbeat_msg, MSG_HEARTBEAT_BROKER);
            zmsg_send(&heartbeat_msg, broker->sock_local_backend);

            worker = (worker_t*)zlist_next(workers);

        };
        broker_unlock_workers(broker);

    }

    broker_workers_purge(broker);

    return 0;
}

/* ================ run_broker() ================ */
int run_broker(const char *frontend, const char *backend, int verbose)
{
    info_log("run_broker() with frontend:%s backend:%s", frontend, backend);

    int rc = 0;
    broker_t *broker = broker_new();

    zsock_t *sock_local_frontend = zsock_new_router(frontend);
    zsock_t *sock_local_backend = zsock_new_router(backend);

    zloop_t *loop = zloop_new();
    zloop_set_verbose(loop, verbose);

    broker->loop = loop;
    broker->sock_local_frontend = sock_local_frontend;
    broker->sock_local_backend = sock_local_backend;

    broker->heartbeat_timer_id = zloop_timer(loop, HEARTBEAT_INTERVAL, -1, handle_heartbeat_timer, broker);

    zloop_reader(loop, sock_local_frontend, handle_pullin_on_local_frontend, broker);
    zloop_reader(loop, sock_local_backend, handle_pullin_on_local_backend, broker);

    zloop_start(loop);

    zsock_destroy(&sock_local_frontend);
    zsock_destroy(&sock_local_backend);

    broker_free(broker);

    return rc;
}


