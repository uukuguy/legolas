/**
 * @file   service.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-10-10 00:34:35
 * 
 * @brief  
 * 
 * 
 */

#include "service.h"
#include "zmalloc.h"
#include "sysinfo.h"
#include "logger.h"

/* ==================== service_new() ==================== */ 
service_t *service_new(void *parent)
{
    service_t *service = (service_t*)zmalloc(sizeof(service_t));
    memset(service, 0, sizeof(service_t));

    service->parent = parent;

    service_init(service);

    return service;
}

/* ==================== service_free() ==================== */ 
void service_free(service_t *service)
{
    service_destroy(service);
    zfree(service);
}

/* FIXME 2014-10-10 11:33:55 */
/* ==================== service_consume_sockbuf() ==================== */ 
void service_consume_sockbuf(coroutine_t *rx_coroutine, sockbuf_t *sockbuf)
{
    session_t *session = sockbuf->session;

    /*pthread_mutex_lock(&session->recv_pending_lock);*/

    if ( likely( sockbuf->remain_bytes > 0 ) ) {
        /* FIXME coroutine */
        session->sockbuf = sockbuf;

        /*coroutine_enter(session, session);*/
        greenlet_switch_to(rx_coroutine, session);
        /*coroutine_enter(rx_coroutine, session);*/


    } else {
        /* -------- remain bytes == 0 -------- */
    }

    sockbuf_free(sockbuf);

    __sync_add_and_fetch(&session->total_saved_buffers, 1);

    /*pthread_yield();*/
    /*sched_yield();*/

    /*pthread_mutex_unlock(&session->recv_pending_lock);*/
}

#include "session.h"
int session_do_read(sockbuf_t *sockbuf, message_t **p_message); /* in sockbuf_message.c */
void *work_queue_rx_coroutine(void *opaque)
{
    trace_log("enter work_queue_rx_coroutine().");
    UNUSED int ret;
    session_t *session = (session_t*)opaque;

    while ( !session->stop ){

        /** ----------------------------------------
         *    Keep read data
         *  ---------------------------------------- */

        /* FIXME coroutine */
        /*session = coroutine_self_data();*/
        sockbuf_t *sockbuf = session->sockbuf;
        message_t *message = NULL;
        ret = session_do_read(sockbuf, &message);

        /* FIXME coroutine */
        /*session = coroutine_self_data();*/
        sockbuf = session->sockbuf;

        /* FIXME coroutine */
        /*sockbuf = coroutine_self_data();*/
        /*session = sockbuf->session;*/

        if ( ret == 0 ) {
            assert(message != NULL);

            if ( session->callbacks.handle_message != NULL ){
                ret = session->callbacks.handle_message(session, message);
            }
            zfree(message);
            message = NULL;
        } else {
            /*YIELD_AND_CONTINUE;*/
            greenlet_t *current = greenlet_current();
            greenlet_t *parent = greenlet_parent(current);
            greenlet_switch_to(parent, NULL);
            sockbuf = session->sockbuf; 
            continue;
        }
    }

    trace_log("leave work_queue_rx_coroutine().");

    return NULL;
}

void set_work_queue_coroutine(work_queue_t *wq, coroutine_t *rx_coroutine);
coroutine_t *get_work_queue_coroutine(work_queue_t *wq); /* in work.c */
/* ==================== parse_queue_handle_request() ==================== */ 
/* Working in work_queue thread. */
 
void session_consume_sockbuf(sockbuf_t *sockbuf); /* in session_sockbuf_message.c */
void parse_queue_handle_request(work_queue_t *wq)
{

    coroutine_t *rx_coroutine = get_work_queue_coroutine(wq);
    if ( rx_coroutine == NULL ){
        rx_coroutine = greenlet_new(work_queue_rx_coroutine, greenlet_current(), 64 * 1024);
        if ( rx_coroutine == NULL ) {
            error_log("Cann't create coroutine session->rx_coroutine");
            return;
        }
        set_work_queue_coroutine(wq, rx_coroutine);
    }

    void *data = NULL;
    while ( (data = dequeue_work(wq)) != NULL ){
        sockbuf_t *sockbuf = (sockbuf_t*)data;

        /* FIXME 2014-10-10 11:34:28 */
        /*session_t *session = sockbuf->session;*/
        /*service_t *service = session->service;*/

        /*int wq_id = work_queue_get_id(wq);*/
        /*coroutine_t *rx_coroutine = service->rx_coroutines[wq_id];*/

        coroutine_t *rx_coroutine = get_work_queue_coroutine(wq);
        trace_log("Before service_consume_sockbuf()");
        service_consume_sockbuf(rx_coroutine, sockbuf);
        trace_log("After service_consume_sockbuf()");

        /*session_consume_sockbuf(sockbuf);*/
    }
}

void *session_rx_coroutine(void *opaque); /* in sockbuf_message.c */
/* ==================== service_init() ==================== */ 
int service_init(service_t *service)
{
    sysinfo_t sysinfo;
    flush_sysinfo(&sysinfo);
    /* FIXME 2014-10-10 01:53:42 */
    /*service->num_processors = sysinfo.num_processors;*/
    service->num_processors = 1;

    /* -------- parse_queue -------- */
    uint32_t parse_threads = service->num_processors;
    service->parse_threads = parse_threads;

    /*FIXME 2014-10-10 01:35:57 */
    service->parse_queue = (work_queue_t**)zmalloc(sizeof(work_queue_t*) * parse_threads);
    /* FIXME 2014-10-10 11:34:43 */
    /*service->rx_coroutines = (coroutine_t**)zmalloc(sizeof(coroutine_t*) * parse_threads);*/

    /* FIXME 2014-10-10 01:36:27 */
    int i;
    for ( i = 0; i < parse_threads; i++ ) {

        service->parse_queue[i] = init_work_queue(parse_queue_handle_request, PARSE_INTERVAL);
        if ( service->parse_queue[i] == NULL ){
            return -1;
        }

        work_queue_set_id(service->parse_queue[i], i);

        /*service->rx_coroutines[i] = greenlet_new(session_rx_coroutine, greenlet_current(), 64 * 1024);*/
        /*service->rx_coroutines[i] = coroutine_create(session_rx_coroutine);*/
        /*if ( service->rx_coroutines[i] == NULL ) {*/
            /*error_log("Cann't create coroutine session->rx_coroutine");*/
            /*return -1;*/
        /*}*/
    }

    return 0;
}

/* ==================== service_destroy() ==================== */ 
void service_destroy(service_t *service)
{
    /* -------- parse_queue -------- */
    /* FIXME 2014-10-10 01:36:47 */
    uint32_t parse_threads = service->parse_threads;
    int i;
    for ( i = 0; i < parse_threads; i++ ) {
        work_queue_t *wq = service->parse_queue[i];
        if ( wq != NULL ) {
            exit_work_queue(wq);
            zfree(wq);
            service->parse_queue[i] = NULL;
        }

        /* FIXME 2014-10-10 11:35:10 */
        /*if ( service->rx_coroutines[i] != NULL ){*/
            /*greenlet_destroy(service->rx_coroutines[i]);*/
            /*coroutine_delete(service->rx_coroutines[i]);*/
            /*service->rx_coroutines[i] = NULL;*/
        /*}*/
    }
    zfree(service->parse_queue);
    service->parse_queue = NULL;

    /* FIXME 2014-10-10 11:35:28 */
    /*zfree(service->rx_coroutines);*/
    /*service->rx_coroutines = NULL;*/

}

/* ==================== get_parse_queue_by_session() ==================== */ 
work_queue_t *get_parse_queue_by_session(session_t *session)
{
    service_t *service = session->service;
    uint32_t parse_threads = service->parse_threads;

    int fd = session_fd(session);    
    int idx = fd % parse_threads;
    assert(idx >= 0 && idx < parse_threads);
    return service->parse_queue[idx];
}

/* ==================== enqueue_parse_queue() ==================== */ 
void enqueue_parse_queue(session_t *session, sockbuf_t *sockbuf)
{
    work_queue_t *wq = get_parse_queue_by_session(session);

    if ( wq != NULL ) {
        enqueue_work(wq, (void*)sockbuf);
    }
}

/* ==================== dequeue_parse_queue() ==================== */ 
sockbuf_t *dequeue_parse_queue(session_t *session)
{
    work_queue_t *wq = get_parse_queue_by_session(session);

    if ( wq != NULL ) {
        return dequeue_work(wq);
    }

    return NULL;
}

