/**
 * @file   session_handle.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-07-07 16:15:19
 * 
 * @brief  
 * 
 * 
 */

#include "session.h"
#include "logger.h"

int session_handle_write(session_t *session, message_t *message);
int session_handle_read(session_t *session, message_t *message);
int session_handle_delete(session_t *session, message_t *message);

int session_handle_message(session_t *session, message_t *message)
{
    int ret = 0;

    if ( message->msg_type == MSG_TYPE_REQUEST ){
        switch ( message->op_code ){
            case MSG_OP_WRITE:
                {
                    ret = session_handle_write(session, message);
                } break;
            case MSG_OP_READ:
                {
                    ret = session_handle_read(session, message);
                } break;
            case MSG_OP_DEL:
                {
                    ret = session_handle_delete(session, message);
                } break;
        };
    } else if ( message->msg_type == MSG_TYPE_RESPONSE ) {
        switch ( message->op_code ){
            case MSG_OP_WRITE:
                {
                    ret = session_handle_write(session, message);
                } break;
            case MSG_OP_READ:
                {
                    ret = session_handle_read(session, message);
                } break;
            case MSG_OP_DEL:
                {
                    ret = session_handle_delete(session, message);
                } break;
        };
    }

    return ret;
}

/* ==================== session_is_idle() ==================== */ 
int session_is_idle(session_t *session)
{
    assert(session->total_received_buffers >= session->total_saved_buffers);

    int ret = 0;

    if ( session->total_received_buffers == 
            session->total_saved_buffers ){
        ret = 1;
    } else {
        ret = 0;
    }

    return ret;
}

void session_idle_cb(uv_idle_t *idle_handle, int status) 
{
    session_t *session = (session_t*)idle_handle->data;
    session_shutdown(session);

}

/* ==================== session_timer_cb() ==================== */ 
void session_timer_cb(uv_timer_t *timer_handle, int status) 
{
}

/* ==================== session_async_cb() ==================== */ 
void session_async_cb(uv_async_t *async_handle, int status) 
{
}

int session_init(session_t *session)
{
    return 0;
}

void session_destroy(session_t *session) 
{
}

