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

/* ==================== parse_queue_handle_request() ==================== */ 
void session_consume_sockbuf(sockbuf_t *sockbuf); /* in session_sockbuf_message.c */
void parse_queue_handle_request(work_queue_t *wq)
{
    void *data = NULL;
    while ( (data = dequeue_work(wq)) != NULL ){
        sockbuf_t *sockbuf = (sockbuf_t*)data;
        session_consume_sockbuf(sockbuf);
    }
}

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

    service->parse_queue = (work_queue_t**)zmalloc(sizeof(work_queue_t*) * parse_threads);

    int i;
	for ( i = 0; i < parse_threads; i++ ) {
		service->parse_queue[i] = init_work_queue(parse_queue_handle_request, PARSE_INTERVAL);
		if ( service->parse_queue[i] == NULL ){
			return -1;
        }
	}

    return 0;
}

/* ==================== service_destroy() ==================== */ 
void service_destroy(service_t *service)
{
    /* -------- parse_queue -------- */
    uint32_t parse_threads = service->parse_threads;
    int i;
	for ( i = 0; i < parse_threads; i++ ) {
        work_queue_t *wq = service->parse_queue[i];
        if ( wq != NULL ) {
            exit_work_queue(wq);
            zfree(wq);
            service->parse_queue[i] = NULL;
        }
	}
    zfree(service->parse_queue);
    service->parse_queue = NULL;
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

