/**
 * @file   session_handle.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-07-07 16:15:19
 * 
 * @brief  
 * 
 * 
 */

#include "session_handle.h"
#include "session.h"
#include "logger.h"

#include "server.h"
#include "kvdb.h"
#include "vnode.h"

int server_handle_write(session_t *session, message_t *message);
int server_handle_read(session_t *session, message_t *message);
int server_handle_delete(session_t *session, message_t *message);

typedef int (session_handle_func_t)(session_t*, message_t*);
typedef struct session_handle_args_t {
    session_t *session;
    message_t *message;
    session_handle_func_t *handle_func;
} session_handle_args_t;


void *session_handle_routine(void *data)
{
    session_handle_args_t *args = (session_handle_args_t*)data;
    session_t *session = args->session;
    message_t *message = args->message;
    session_handle_func_t *handle_func = args->handle_func;

    handle_func(session, message);

    return NULL;
}

int start_session_handle(session_handle_args_t *args)
{

    /*-------- pth -------- */
    pth_attr_t attr = pth_attr_new();
    pth_attr_set(attr, PTH_ATTR_JOINABLE, 0);
    pth_t tid = pth_spawn(attr, session_handle_routine, args);
    if ( tid == NULL ) {
        error_log("pth_spawn() failed.");
    }
    pth_attr_destroy(attr);
    pth_yield(NULL);
    return 0;
}

int server_handle_message(session_t *session, message_t *message)
{
    server_t *server = server(session);
    UNUSED uint32_t total_requests = __sync_add_and_fetch(&server->total_requests, 1);

    int ret = 0;

    if ( message->msg_type == MSG_TYPE_REQUEST ){
        switch ( message->op_code ){
            case MSG_OP_WRITE:
                {
                    ret = server_handle_write(session, message);
                    /*session_handle_args_t args = {*/
                        /*.session = session,*/
                        /*.message = message,*/
                        /*.handle_func = server_handle_write,*/
                    /*};*/

                    /*ret = start_session_handle(&args);*/

                } break;
            case MSG_OP_READ:
                {
                    ret = server_handle_read(session, message);

                    /*session_handle_args_t args = {*/
                        /*.session = session,*/
                        /*.message = message,*/
                        /*.handle_func = session_handle_read,*/
                    /*};*/

                    /*ret = start_session_handle(&args);*/

                } break;
            case MSG_OP_DEL:
                {
                    ret = server_handle_delete(session, message);

                    /*session_handle_args_t args = {*/
                        /*.session = session,*/
                        /*.message = message,*/
                        /*.handle_func = session_handle_delete,*/
                    /*};*/

                    /*ret = start_session_handle(&args);*/

                } break;
        };
    } else if ( message->msg_type == MSG_TYPE_RESPONSE ) {
    }

    /*if ( total_requests >= 30000 ) {*/
        /*notice_log("Stop! server->total_requests(%d) >= 10000", total_requests);*/
        /*uv_loop_t *loop = &(server->connection.loop);*/
        /*uv_stop(loop);*/
    /*}*/

    return ret;
}

/* ==================== server_is_idle() ==================== */ 
int server_is_idle(session_t *session)
{
    assert(session->total_received_buffers >= session->total_saved_buffers);

    int ret = 0;

    if ( session->total_received_buffers == 
            session->total_saved_buffers && session->running_tasks == 0 && session->finished_works == 0){
        ret = 1;
    } else {
        ret = 0;
    }

    return ret;
}

/* ==================== server_idle_cb() ==================== */ 
void server_idle_cb(uv_idle_t *idle_handle, int status) 
{
    session_t *session = (session_t*)idle_handle->data;

    /* FIXME Response success to client. for write.*/
    uint32_t finished_works = session->finished_works;
    if ( finished_works > 0 ) {
        __sync_sub_and_fetch(&session->finished_works, finished_works);
        __sync_add_and_fetch(&session->total_finished_works, 1);
        while ( finished_works-- > 0 ) {
            __sync_sub_and_fetch(&session->running_tasks, 1);
            if ( session->waiting_for_close == 0 )
                session_response(session, RESULT_SUCCESS);
        }
    }

    /* Check is time to shutdown now? */
    session_shutdown(session);

}

/* ==================== session_timer_cb() ==================== */ 
void session_timer_cb(uv_timer_t *timer_handle, int status) 
{
    /*session_t *session = timer_handle->data;*/

    /*server_t *server = SERVER(session);*/

    /*int n;*/
    /*for ( n = 0 ; n < VNODES ; n++ ){*/
        /*kvdb_flush(server->vnodes[n]->kvdb);*/
    /*}*/
}

/* ==================== session_async_cb() ==================== */ 
void session_async_cb(uv_async_t *async_handle, int status) 
{
}

/* ==================== session_init() ==================== */ 
int session_init(session_t *session)
{
    return 0;
}

/* ==================== session_destroy() ==================== */ 
void session_destroy(session_t *session) 
{
    if ( session != NULL ){
        zfree(session);
    }
}

/* ==================== response_with_key() ==================== */ 
void response_with_key(session_t *session, msgidx_t *msgidx, int result)
{
    message_t *response = alloc_response_message(result);
    response = add_message_arg(response, msgidx->key, msgidx->keylen);
    uint32_t msg_size = sizeof(message_t) + response->data_length;

    session_response_data(session, (char *)response, msg_size);

    zfree(response);
}

