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

typedef struct service_t
{
    void *parent;

    uint32_t num_processors;
    uint32_t parse_threads;

    work_queue_t **parse_queue;

    /* FIXME 2014-10-10 11:35:51 */
    coroutine_t **rx_coroutines;


} service_t;

service_t *service_new(void *parent);
void service_free(service_t *service);

int service_init(service_t *service);
void service_destroy(service_t *service);

void enqueue_parse_queue(session_t *session, sockbuf_t *sockbuf);
sockbuf_t *dequeue_parse_queue(session_t *session);

#endif /* __SERVICE_H__ */

