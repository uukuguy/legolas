/**
 * @file   service.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-10-10 00:31:43
 * 
 * @brief  
 * 
 * 
 */

#ifndef __SERVICE_H__
#define __SERVICE_H__

#include "common.h"
#include "work.h"
#include "session.h"
#include <uv.h>

#define PARSE_INTERVAL 1 /* ms */

/* -------------------- session_callbacks_t -------------------- */


typedef int (*is_idle_cb)(session_t*);
typedef int (*handle_message_cb)(session_t*, message_t *);
typedef int (*session_init_cb)(session_t*);
typedef void (*session_destroy_cb)(session_t*);
typedef void (*consume_sockbuf_cb)(sockbuf_t*);
typedef void (*on_connect_cb)(uv_connect_t*, int); 
typedef int (*handle_read_response_cb)(session_t*, msgidx_t*);
typedef void (*session_on_close_cb)(session_t*);

//typedef void (*session_on_connect_cb)(service_t*, int);
typedef void (*session_on_connect_to_server_cb)(session_t*, int);
typedef void (*session_on_connect_from_client_cb)(service_t*, int);

typedef struct session_callbacks_t {
    uv_idle_cb idle_cb;
    uv_timer_cb  timer_cb;
    uv_async_cb async_cb;
    is_idle_cb is_idle;
    handle_message_cb handle_message;
    session_init_cb session_init;
    session_destroy_cb session_destroy;
    consume_sockbuf_cb consume_sockbuf;
    on_connect_cb on_connect;
    handle_read_response_cb handle_read_response;

    session_on_connect_to_server_cb session_on_connect_to_server;
    session_on_connect_from_client_cb session_on_connect_from_client;

    session_on_close_cb session_on_close;

} session_callbacks_t;

typedef struct service_t
{
    void *parent;

    connection_t connection;

    uint32_t num_processors;
    uint32_t parse_threads;

    work_queue_t **parse_queue;

    session_callbacks_t callbacks;
    /* FIXME 2014-10-10 11:35:51 */
    //coroutine_t **rx_coroutines;


} service_t;

service_t *service_new(void *parent, const session_callbacks_t *callbacks);
void service_free(service_t *service);

int service_init(service_t *service);
void service_destroy(service_t *service);

void enqueue_parse_queue(session_t *session, sockbuf_t *sockbuf);
sockbuf_t *dequeue_parse_queue(session_t *session);

void service_stop(service_t *service);

#endif /* __SERVICE_H__ */

