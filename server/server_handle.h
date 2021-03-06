/**
 * @file   server_handle.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-07-07 00:45:04
 * 
 * @brief  
 * 
 * 
 */

#ifndef __SERVER_HANDLE_H__
#define __SERVER_HANDLE_H__

#include "md5.h"
#include <uv.h>

typedef struct session_t session_t;
typedef struct message_t message_t;
typedef struct msgidx_t msgidx_t;

void server_idle_cb(uv_idle_t *idle_handle, int status);
int server_is_idle(session_t *session);

void session_timer_cb(uv_timer_t *timer_handle, int status);
void session_async_cb(uv_async_t *async_handle, int status);
int server_handle_message(session_t *session, message_t *message);
int session_init(session_t *session);
void session_destroy(session_t *session);

void response_with_key(session_t *session, msgidx_t *msgidx, int result);

#endif /* __SERVER_HANDLE_H__ */

