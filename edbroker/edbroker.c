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
#include "common.h"
#include "logger.h"

#define WORKER_READY "\001"

typedef struct broker_t{
    zloop_t *loop;
    zsock_t *sock_local_frontend;
    zsock_t *sock_local_backend;
    int timer_id;

    zlist_t *workers;
    uint32_t available_workers;
} broker_t;

broker_t *broker_new(void)
{
    broker_t *broker = (broker_t*)malloc(sizeof(broker_t));
    memset(broker, 0, sizeof(broker_t));

    broker->timer_id = -1;
    broker->workers = zlist_new();

    return broker;
}

void broker_free(broker_t *broker)
{
    zlist_destroy(&broker->workers);
    free(broker);
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
    if ( broker->timer_id != -1 ){
        zloop_timer_end(broker->loop, broker->timer_id);
        broker->timer_id = -1;
    }
}

void broker_end_loop(broker_t *broker)
{
    broker_end_local_frontend(broker);
    broker_end_local_backend(broker);
    broker_end_timer(broker);
}

int handle_read_local_frontend(zloop_t *loop, zsock_t *sock, void *user_data)
{
    broker_t *broker = (broker_t*)user_data;

    if ( broker->available_workers > 0 ){
        zmsg_t *msg = zmsg_recv(sock);
        if ( msg == NULL ){
            broker_end_loop(broker);
            return -1;
        }
        /*zmsg_print(msg);*/

        zframe_t *identity = (zframe_t*)zlist_pop(broker->workers);
        zmsg_wrap(msg, identity);
        zmsg_send(&msg, broker->sock_local_backend);

        broker->available_workers--;
    }

    return 0;
}

int handle_read_local_backend(zloop_t *loop, zsock_t *sock, void *user_data)
{
    broker_t *broker = (broker_t*)user_data;

    zmsg_t *msg = zmsg_recv(sock);
    if ( msg == NULL ){
        broker_end_loop(broker);
        return -1;
    }
    /*zmsg_print(msg);*/

    zframe_t *identity = zmsg_unwrap(msg);
    zlist_append(broker->workers, identity);
    broker->available_workers++;

    zframe_t *first_frame = zmsg_first(msg);
    if ( memcmp(zframe_data(first_frame), WORKER_READY, 1) == 0 ){
        notice_log("WORKER_READY");
        zmsg_destroy(&msg);
    }
    if ( msg != NULL ){
        zmsg_send(&msg, broker->sock_local_frontend);
    }

    return 0;
}

int handle_timer(zloop_t *loop, int timer_id, void *user_data)
{
    UNUSED broker_t *broker = (broker_t*)user_data;

    return 0;
}

int run_broker(const char *frontend, const char *backend)
{
    info_log("run_broker() with frontend:%s backend:%s", frontend, backend);

    int rc = 0;
    int verbose = 0;
    broker_t *broker = broker_new();

    zsock_t *sock_local_frontend = zsock_new_router(frontend);
    zsock_t *sock_local_backend = zsock_new_router(backend);

    zloop_t *loop = zloop_new();
    zloop_set_verbose(loop, verbose);

    broker->loop = loop;
    broker->sock_local_frontend = sock_local_frontend;
    broker->sock_local_backend = sock_local_backend;

    /*int timer_id = zloop_timer(loop, 1000, 0, handle_timer, broker);*/
    /*broker->timer_id = timer_id;*/

    zloop_reader(loop, sock_local_frontend, handle_read_local_frontend, broker);
    zloop_reader(loop, sock_local_backend, handle_read_local_backend, broker);

    zloop_start(loop);

    zsock_destroy(&sock_local_frontend);
    zsock_destroy(&sock_local_backend);

    broker_free(broker);

    return rc;
}


