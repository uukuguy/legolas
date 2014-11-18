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

#include "containers.h" 

/* -------- struct g_iterator_t -------- */
typedef struct g_iterator_t{
    Iterator *it;
} g_iterator_t;

/* -------- struct g_array_t -------- */
typedef struct g_array_t{
    Vector *vector;
} g_array_t;

/* -------- struct g_dlist_t -------- */
typedef struct g_dlist_t{
    Dlist *dlist;
} g_dlist_t;


/* ================ g_iterator_new() ================ */
/*g_iterator_t *g_iterator_new(g_array_t *array)*/
/*{*/
    /*g_iterator_t *g_iter = (g_iterator_t*)malloc(sizeof(g_iterator_t));*/
    /*memset(g_iter, 0, sizeof(g_iterator_t));*/

    /*g_iter->it = iVector.NewIterator(array->vector);*/

    /*return g_iter;*/
/*}*/

/* ================ g_iterator_free() ================ */
/*void g_iteartor_free(g_iterator_t *g_iter)*/
/*{*/
    /*iVector.DeleteIterator(g_iter->it);*/
    /*free(g_iter);*/
/*}*/

/* ================ g_iterator_get_first() ================ */
void *g_iterator_get_first(g_iterator_t *g_iter)
{
    return g_iter->it->GetFirst(g_iter->it);
}

/* ================ g_iterator_get_next() ================ */
void *g_iterator_get_next(g_iterator_t *g_iter)
{
    return g_iter->it->GetNext(g_iter->it);
}

/* ================ g_iterator_get_previous() ================ */
void *g_iterator_get_previous(g_iterator_t *g_iter)
{
    return g_iter->it->GetPrevious(g_iter->it);
}

/* ================ g_iterator_get_last() ================ */
void *g_iterator_get_last(g_iterator_t *g_iter)
{
    return g_iter->it->GetLast(g_iter->it);
}

/* ================ g_iterator_get_item() ================ */
void *g_iterator_get_item(g_iterator_t *g_iter)
{
    return g_iter->it->GetCurrent(g_iter->it);
}

/* ================ g_iterator_set_item() ================ */
int g_iterator_set_item(g_iterator_t *g_iter, void *new_item)
{
    return g_iter->it->Replace(g_iter->it, new_item, 1); 
}

g_dlist_t *g_dlist_new(size_t item_size)
{
    g_dlist_t *g_dlist = (g_dlist_t*)malloc(sizeof(g_dlist_t));
    memset(g_dlist, 0, sizeof(g_dlist_t));

    g_dlist->dlist = iDlist.Create(item_size);

    return g_dlist;
}

void g_dlist_free(g_dlist_t *g_dlist)
{
    iDlist.Finalize(g_dlist->dlist);
    free(g_dlist);
}

size_t g_dlist_size(g_dlist_t *g_dlist)
{
    return iDlist.Size(g_dlist->dlist);
}

int g_dlist_push_back(g_dlist_t *g_dlist, void *item)
{
    return iDlist.PushBack(g_dlist->dlist, item);
}

int g_dlist_push_front(g_dlist_t *g_dlist, void *item)
{
    return iDlist.PushFront(g_dlist->dlist, item);
}

void *g_dlist_pop_front(g_dlist_t *g_dlist)
{
    void *item = NULL;

    iDlist.PopFront(g_dlist->dlist, &item);

    return item;
}

void *g_dlist_pop_back(g_dlist_t *g_dlist)
{
    void *item = NULL;

    iDlist.PopBack(g_dlist->dlist, &item);

    return item;
}

void g_dlist_clear(g_dlist_t *g_dlist)
{
    iDlist.Clear(g_dlist->dlist);
}

/* ================ g_dlist_erase_at() ================ */
int g_dlist_erase_at(g_dlist_t *g_dlist, size_t item_pos)
{
    return iDlist.EraseAt(g_dlist->dlist, item_pos);
}

void *g_dlist_get_at(g_dlist_t *g_dlist, size_t item_pos)
{
    return iDlist.GetElement(g_dlist->dlist, item_pos);
}

void *g_dlist_get_front(g_dlist_t *g_dlist)
{
    return iDlist.Front(g_dlist->dlist);
}

void *g_dlist_get_back(g_dlist_t *g_dlist)
{
    return iDlist.Back(g_dlist->dlist);
}

/* ================ g_dlist_new_iterator() ================ */
g_iterator_t *g_dlist_new_iterator(g_dlist_t *g_dlist)
{
    g_iterator_t *g_iter = (g_iterator_t*)malloc(sizeof(g_iterator_t));
    memset(g_iter, 0, sizeof(g_iterator_t));

    g_iter->it = iDlist.NewIterator(g_dlist->dlist);

    return g_iter;
}

/* ================ g_dlist_free_iterator() ================ */
void g_dlist_free_iterator(g_iterator_t *g_iter)
{
    iDlist.DeleteIterator(g_iter->it);
    free(g_iter);
}

/* ================ g_array_new() ================ */
g_array_t *g_array_new(size_t item_size, size_t default_items)
{
    g_array_t *array = (g_array_t*)malloc(sizeof(g_array_t));
    memset(array, 0, sizeof(g_array_t));

    array->vector = iVector.Create(item_size, default_items);

    return array;
}

/* ================ g_array_free() ================ */
void g_array_free(g_array_t *array)
{
    iVector.Finalize(array->vector);
    free(array);
}

/* ================ g_array_clear() ================ */
void g_array_clear(g_array_t *array)
{
    iVector.Clear(array->vector);
}

/* ================ g_array_size() ================ */
size_t g_array_size(g_array_t *array)
{
    return iVector.Size(array->vector);
}

/* ================ g_array_push_back() ================ */
int g_array_push_back(g_array_t *array, void *item)
{
    int pos = iVector.Add(array->vector, item);
    /*info_log("g_array_push_back(). item: %p array size: %zu", item, g_array_size(array));*/
    return pos;
}

/* ================ g_array_push_front() ================ */
int g_array_push_front(g_array_t *array, void *item)
{
    int pos = iVector.InsertAt(array->vector, 0, item);
    return pos;
}

/* ================ g_array_pop_front() ================ */
void *g_array_pop_front(g_array_t *array)
{
   void *item = NULL;
   size_t old_size = g_array_size(array);
   /*info_log("g_array_pop_front(). size=%zu", old_size)*/

   if ( g_array_size(array) > 0 ) {
       item = iVector.GetElement(array->vector, 0);
       if ( item != NULL ){
           iVector.EraseAt(array->vector, 0);
       }
       assert(g_array_size(array) == old_size - 1);
   }
   /*info_log("exit g_array_pop_front(). now size = %zu", g_array_size(array));*/
   return item;
}

/* ================ g_array_pop_back() ================ */
void *g_array_pop_back(g_array_t *array)
{
   void *item;
   int rc = iVector.PopBack(array->vector, &item);
   if ( rc > 0 ){
       return item;
   } else {
       return NULL;
   }
}

/* ================ g_array_get_at() ================ */
void *g_array_get_at(g_array_t *array, size_t item_pos)
{
    return iVector.GetElement(array->vector, item_pos);
}

/* ================ g_array_get_front() ================ */
void *g_array_get_front(g_array_t *array)
{
    return iVector.Front(array->vector);
}

/* ================ g_array_get_back() ================ */
void *g_array_get_back(g_array_t *array)
{
    return iVector.Back(array->vector);
}

/* ================ g_array_erase_at() ================ */
int g_array_erase_at(g_array_t *array, size_t item_pos)
{
    return iVector.EraseAt(array->vector, item_pos);
}

/* ================ g_array_new_iterator() ================ */
g_iterator_t *g_array_new_iterator(g_array_t *array)
{
    g_iterator_t *g_iter = (g_iterator_t*)malloc(sizeof(g_iterator_t));
    memset(g_iter, 0, sizeof(g_iterator_t));

    g_iter->it = iVector.NewIterator(array->vector);

    return g_iter;
}

/* ================ g_array_free_iterator() ================ */
void g_array_free_iteartor(g_iterator_t *g_iter)
{
    iVector.DeleteIterator(g_iter->it);
    free(g_iter);
}

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
    if ( worker->id_string != NULL ){
        free(worker->id_string);
        worker->id_string = NULL;
    }
    free(worker);
}

/* -------- struct broker_t -------- */
typedef struct broker_t{
    zloop_t *loop;
    zsock_t *sock_local_frontend;
    zsock_t *sock_local_backend;
    int heartbeat_timer_id;
    int64_t heartbeat_at;

    /*zlist_t *workers;*/
    pthread_mutex_t workers_lock;

    /*g_array_t *backends;*/
    /*g_dlist_t *backends;*/
    zlist_t *backends;

} broker_t;

broker_t *broker_new(void)
{
    broker_t *broker = (broker_t*)malloc(sizeof(broker_t));
    memset(broker, 0, sizeof(broker_t));

    pthread_mutex_init(&broker->workers_lock, NULL);

    broker->heartbeat_timer_id = -1;
    broker->heartbeat_at = zclock_time() + HEARTBEAT_INTERVAL;
    /*broker->workers = zlist_new();*/

    /*broker->backends = g_array_new(sizeof(worker_t*), 4);*/
    /*broker->backends = g_dlist_new(sizeof(worker_t*));*/
    broker->backends = zlist_new();

    return broker;
}

void broker_lock_workers(broker_t *broker)
{
    pthread_mutex_lock(&broker->workers_lock);
}

void broker_unlock_workers(broker_t *broker)
{
    pthread_mutex_unlock(&broker->workers_lock);
}

void broker_free(broker_t *broker)
{

    broker_lock_workers(broker);

    /*zlist_t *workers = broker->workers;*/
    /*while ( zlist_size(workers) ){*/
        /*worker_t *w = (worker_t*)zlist_pop(workers);*/
        /*worker_free(w);*/
    /*}*/

    zlist_t *backends = broker->backends;
    while ( zlist_size(backends) ){
        worker_t *w = (worker_t*)zlist_pop(backends);
        worker_free(w);
    }
    /*g_iterator_t *it = g_array_new_iterator(broker->backends);*/
    /*g_iterator_t *it = g_dlist_new_iterator(broker->backends);*/

    /*for (worker_t **worker_p = (worker_t**)g_iterator_get_first(it);*/
            /*worker_p != NULL ;*/
            /*worker_p = (worker_t**)g_iterator_get_next(it)){*/
        /*worker_t *worker = *worker_p;*/
        /*worker_free(worker);*/
    /*}*/

    /*g_array_free(broker->backends);*/
    /*g_dlist_free(broker->backends);*/

    /*zlist_destroy(&workers);*/
    zlist_destroy(&backends);

    broker_unlock_workers(broker);

    pthread_mutex_destroy(&broker->workers_lock);


    free(broker);
}

uint32_t broker_get_available_workers(broker_t *broker)
{
    /*return zlist_size(broker->workers);*/
    /*return g_array_size(broker->backends);*/
    /*return g_dlist_size(broker->backends);*/
    return zlist_size(broker->backends);
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

    /* FIXME 2014-11-18 00:30:05 */
    /*size_t idx = 0;*/
    /*size_t total_workers = broker_get_available_workers(broker);*/
    /*if ( total_workers > 0 ){*/
        /*worker_t *w = NULL;*/
        /*[>worker_t **w_p = (worker_t**)g_array_get_at(broker->backends, 0);<]*/
        /*[>worker_t **w_p = (worker_t**)g_dlist_get_at(broker->backends, 0);<]*/

        /*size_t old_size = g_dlist_size(broker->backends);*/
        /*g_iterator_t *it = g_dlist_new_iterator(broker->backends);*/
        /*worker_t **w_p = g_iterator_get_first(it);*/

        /*if ( w_p != NULL )*/
            /*w = *w_p;*/

        /*while ( w != NULL ){*/

            /*if ( w->id_string != NULL && worker->id_string != NULL && strcmp(worker->id_string, w->id_string) == 0 ){*/
                /*[>g_array_erase_at(broker->backends, idx);<]*/
                /*g_dlist_erase_at(broker->backends, idx);*/
                /*size_t now_size = g_dlist_size(broker->backends);*/
                /*assert(now_size == old_size - 1);*/
                /*worker_free(w);*/
                /*break;*/
            /*}*/

            /*idx++;*/
            /*if ( idx >= total_workers )*/
                /*break;*/

            /*w = NULL;*/
            /*[>w_p = (worker_t**)g_array_get_at(broker->backends, idx);<]*/
            /*w_p = g_iterator_get_next(it);*/
            /*if ( w_p != NULL ) */
                /*w = *w_p;*/
        /*}*/

        /*g_dlist_free_iterator(it);*/
    /*}*/

    /*worker_t **w_p = &worker;*/
    /*g_array_push_back(broker->backends, (void*)w_p);*/
    /*g_dlist_push_back(broker->backends, (void*)w_p);*/

    /*info_log("g_array_push_back(). worker:%p", worker);*/


    /*zlist_t *workers = broker->workers;*/

    /*worker_t *w = (worker_t*)zlist_first(workers);*/
    /*while ( w != NULL ){*/
        /*if ( strcmp(worker->id_string, w->id_string) == 0 ){*/
            /*zlist_remove(workers, w);*/
            /*worker_free(w);*/
            /*break;*/
        /*}*/
        /*w = (worker_t*)zlist_next(workers);*/
    /*};*/
    /*zlist_append(workers, worker);*/

    zlist_t *backends = broker->backends;

    worker_t *w = (worker_t*)zlist_first(backends);
    while ( w != NULL ){
        if ( strcmp(worker->id_string, w->id_string) == 0 ){
            zlist_remove(backends, w);
            worker_free(w);
            break;
        }
        w = (worker_t*)zlist_next(backends);
    };
    zlist_append(backends, worker);

    broker_unlock_workers(broker);
}

/* ================ broker_workers_pop_without_lock() ================ */
zframe_t *broker_workers_pop_without_lock(broker_t *broker)
{
    zframe_t *identity = NULL;


    /* FIXME 2014-11-18 00:46:36 */
    /*zlist_t *workers = broker->workers;*/
    /*worker_t *worker = (worker_t*)zlist_pop(workers);*/
    zlist_t *backends = broker->backends;
    worker_t *worker = (worker_t*)zlist_pop(backends);

    /*[>worker_t **worker_p = (worker_t**)g_array_pop_front(broker->backends);<]*/
    /*worker_t **worker_p = (worker_t**)g_dlist_pop_front(broker->backends);*/

    /*[>info_log("worker_p: %p", worker_p);<]*/
    /*if ( worker_p == NULL ){*/
        /*return NULL;*/
    /*}*/
    /*worker_t *worker = *worker_p;*/
    /*[>info_log("worker_p: %p, worker: %p", worker_p, worker);<]*/

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

    broker_lock_workers(broker);

    /* FIXME 2014-11-18 00:49:31 */
    /*zlist_t *workers = broker->workers;*/
    /*worker_t *w = (worker_t*)zlist_first(workers);*/
    zlist_t *backends = broker->backends;
    worker_t *w = (worker_t*)zlist_first(backends);

    /*worker_t *w = NULL;*/
    /*[>worker_t **w_p = (worker_t**)g_array_get_front(broker->backends);<]*/
    /*worker_t **w_p = (worker_t**)g_dlist_get_front(broker->backends);*/

    /*[>g_iterator_t *it = g_dlist_new_iterator(broker->backends);<]*/
    /*[>worker_t **w_p = (worker_t**)g_iterator_get_first(it);<]*/

    /*if ( w_p != NULL )*/
        /*w = *w_p;*/

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

        /* FIXME 2014-11-18 00:50:21 */
        w = (worker_t*)zlist_first(backends);
        /*w = NULL;*/
        /*[>w_p = (worker_t**)g_array_get_front(broker->backends);<]*/
        /*w_p = (worker_t**)g_dlist_get_front(broker->backends);*/
        /*if ( w_p != NULL )*/
            /*w = *w_p;*/
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

        /* FIXME 2014-11-18 00:52:56 */
        /*worker_t *w = (worker_t*)zlist_first(broker->workers);*/
        worker_t *w = (worker_t*)zlist_first(broker->backends);

        /*worker_t *w = NULL;*/
        /*[>worker_t **w_p = (worker_t**)g_array_get_front(broker->backends);<]*/
        /*worker_t **w_p = (worker_t**)g_dlist_get_front(broker->backends);*/
        /*if ( w_p != NULL )*/
            /*w = *w_p;*/

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

    if ( zclock_time() >= broker->heartbeat_at ){

        broker->heartbeat_at = zclock_time() + HEARTBEAT_INTERVAL;

        broker_lock_workers(broker);

        /* FIXME 2014-11-18 00:54:20 */
        /*zlist_t *workers = broker->workers;*/
        /*assert(workers != NULL);*/
        /*worker_t *worker = (worker_t*)zlist_first(workers);*/

        zlist_t *backends = broker->backends;
        worker_t *worker = (worker_t*)zlist_first(backends);

        /*[>g_iterator_t *it = g_array_new_iterator(broker->backends);<]*/
        /*g_iterator_t *it = g_dlist_new_iterator(broker->backends);*/

        /*worker_t *worker = NULL;*/
        /*worker_t **w_p = (worker_t**)g_iterator_get_first(it);*/
        /*if ( w_p != NULL )*/
            /*worker = *w_p;*/

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

            /* FIXME 2014-11-18 00:55:20 */
            /*worker = (worker_t*)zlist_next(workers);*/
            worker = (worker_t*)zlist_next(backends);

            /*worker = NULL;*/
            /*w_p = (worker_t**)g_iterator_get_next(it);*/
            /*if ( w_p != NULL )*/
                /*worker = *w_p;*/

        };
        /*g_array_free_iteartor(it);*/
        /*g_dlist_free_iterator(it);*/

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


