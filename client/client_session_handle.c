/**
 * @file   client_handle.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-07-07 16:31:52
 * 
 * @brief  
 * 
 * 
 */

#include "client.h"
#include "session.h"
#include "logger.h"

/*int session_handle_write(session_t *session, message_t *message);*/
/*int session_handle_read(session_t *session, message_t *message);*/
/*int session_handle_delete(session_t *session, message_t *message);*/

/* ==================== client_session_is_idle() ==================== */ 
int client_session_is_idle(session_t *session)
{
    return 1;
    /*assert(session->total_received_buffers >= session->total_saved_buffers);*/

    /*int ret = 1;*/

    /*trace_log("total_received_buffers:%d total_saved_buffers=%d", session->total_received_buffers, session->total_saved_buffers);*/
    /*if ( session->total_received_buffers == */
            /*session->total_saved_buffers ){*/
        /*ret = 1;*/
    /*} else {*/
        /*ret = 0;*/
    /*}*/

    /*return ret;*/
}

/* ==================== client_session_idle_cb() ==================== */ 
void client_session_idle_cb(uv_idle_t *idle_handle, int status) 
{
    /*session_t *session = (session_t*)idle_handle->data;*/

    /*uint32_t finished_works = session->finished_works;*/
    /*if ( finished_works > 0 ) {*/
        /*__sync_sub_and_fetch(&session->finished_works, finished_works);*/
        /*while ( finished_works-- > 0 ) {*/
            /*session_response(session, RESULT_SUCCESS);*/
        /*}*/
    /*}*/

    /*if ( session->waiting_for_close == 1 ) {*/
        /*if ( session->callbacks.is_idle(session) ) {*/
            /*info_log("Start to close session in session_idle_cb()");*/
            /*uv_timer_stop(&session->timer_handle);*/

            /*uv_shutdown_t* shutdown_req = (uv_shutdown_t*)zmalloc(sizeof(uv_shutdown_t));*/
            /*shutdown_req->data = session;*/
            /*uv_shutdown(shutdown_req, &session->connection.handle.stream, after_shutdown);*/
            /*return;*/
        /*}*/
    /*}*/
}

/* ==================== client_session_timer_cb() ==================== */ 
void client_session_timer_cb(uv_timer_t *timer_handle, int status) 
{
}

/* ==================== client_session_async_cb() ==================== */ 
void client_session_async_cb(uv_async_t *async_handle, int status) 
{
}

/* ==================== client_session_init() ==================== */ 
int client_session_init(session_t *session)
{
    return 0;
}

/* ==================== client_session_destroy() ==================== */ 
void client_session_destroy(session_t *session) 
{
}

