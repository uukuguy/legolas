/**
 * @file   session_handle.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-07-07 00:45:04
 * 
 * @brief  
 * 
 * 
 */

#ifndef __SESSION_HANDLE_H__
#define __SESSION_HANDLE_H__

#include "md5.h"
#include <uv.h>

typedef struct session_t session_t;
typedef struct message_t message_t;
typedef struct msgidx_t msgidx_t;

void session_idle_cb(uv_idle_t *idle_handle, int status);
void session_timer_cb(uv_timer_t *timer_handle, int status);
void session_async_cb(uv_async_t *async_handle, int status);
int session_is_idle(session_t *session);
int session_handle_message(session_t *session, message_t *message);
int session_init(session_t *session);
void session_destroy(session_t *session);

void response_with_key(session_t *session, msgidx_t *msgidx, int result);

#endif /* __SESSION_HANDLE_H__ */

