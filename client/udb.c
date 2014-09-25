/**
 * @file   udb.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-09-09 13:29:43
 * 
 * @brief  
 * 
 * 
 */

#include "udb.h"
#include "session.h"
#include "legolas.h"

#include "zmalloc.h"
#include "adlist.h"
#include "adlist.h"
#include "uv.h"
#include "logger.h"

static uint32_t udb_id = 0;

int udb_is_write_done(udb_t *udb)
{
    return udb->total_writed < udb->object_size ? 0 : 1;
}

uint32_t udb_get_writed_bytes(udb_t *udb)
{
    return udb->total_writed;
}

uint32_t udb_get_readed_bytes(udb_t *udb)
{
    return udb->total_readed;
}

int udb_do(udb_t *udb, on_ready_cb on_ready)
{
    assert(udb != NULL);
    if ( on_ready != NULL ){
        udb->on_ready = on_ready;
    }

    int ret = 0;

	pthread_mutex_init(&udb->main_pending_lock, NULL);
	pthread_cond_init(&udb->main_pending_cond, NULL);

    udb_run(udb);

    pthread_mutex_lock(&udb->main_pending_lock);
    pthread_cond_wait(&udb->main_pending_cond, &udb->main_pending_lock);
    pthread_mutex_unlock(&udb->main_pending_lock);

    pthread_mutex_destroy(&udb->main_pending_lock);
    pthread_cond_destroy(&udb->main_pending_cond);

    notice_log("--- udb_do() udb(%d) done! ---", udb->id);

    return ret;
}

void udb_done(udb_t *udb)
{
    assert(udb != NULL);

    /* FIXME */
    /* Check is time to shutdown now? */
    session_t *session = udb->session; 
    session_rx_off(session);
    session->waiting_for_close = 1;
    session_shutdown(session);

    /*pthread_cond_signal(&udb->main_pending_cond);*/
}

/* ==================== udb_new() ==================== */ 
udb_t *udb_new(const char *ip, int port, void *user_data)
{

    udb_t *udb = (udb_t*)zmalloc(sizeof(udb_t));
    memset(udb, 0, sizeof(udb_t));

    udb->id = udb_id;
    __sync_add_and_fetch(&udb_id, 1);

    notice_log("udb_new(), udb_id=%d", udb->id);

    udb->ip = ip;
    udb->port = port;

    udb->user_data = user_data;
    udb->writing_objects = listCreate();

	pthread_mutex_init(&udb->on_ready_lock, NULL);
	pthread_cond_init(&udb->on_ready_cond, NULL);

    return udb;
}


void udb_free(udb_t *udb)
{
    if ( udb != NULL ) {

        if ( udb->writing_objects != NULL ){
            listRelease(udb->writing_objects);
            udb->writing_objects = NULL;
        }

        if ( udb->user_data != NULL ){
            zfree(udb->user_data);
            udb->user_data = NULL;
        }

        pthread_mutex_destroy(&udb->on_ready_lock);
        pthread_cond_destroy(&udb->on_ready_cond);

        zfree(udb);
    }
}

/* ==================== udb_open_data() ==================== */ 
int udb_open_data(udb_t *udb, const char *key)
{
    return -1;
}

/* ==================== udb_close_data() ==================== */ 
int udb_close_data(udb_t *udb, int handle)
{
    return -1;
}

int udb_handle_write_response(session_t *session, message_t *response);
int udb_handle_read_response(session_t *session, message_t *response);
int udb_handle_delete_response(session_t *session, message_t *response);

int udb_handle_message(session_t *session, message_t *message)
{
    int ret = 0;
    udb_t *udb = udb(session);

    trace_log("udb_handle_message(). Has a message. op_code:%d", udb->op_code);
    switch ( udb->op_code ){
        case MSG_OP_WRITE:
            {
                if ( message->msg_type == MSG_TYPE_REQUEST ){
                } else if ( message->msg_type == MSG_TYPE_RESPONSE ) {
                    ret = udb_handle_write_response(session, message);
                }
            } break;
        case MSG_OP_READ:
            {
                if ( message->msg_type == MSG_TYPE_REQUEST ){
                } else if ( message->msg_type == MSG_TYPE_RESPONSE ) {
                    ret = udb_handle_read_response(session, message);
                }
            } break;
        case MSG_OP_DEL:
            {
                if ( message->msg_type == MSG_TYPE_REQUEST ){
                } else if ( message->msg_type == MSG_TYPE_RESPONSE ) {
                    ret = udb_handle_delete_response(session, message);
                }
            } break;
    }


    return ret;
}

/* ==================== on_connect() ==================== */ 
static void on_connect(uv_connect_t *req, int status) 
{
    session_t *session = (session_t*)req->handle->data;
    UNUSED udb_t *udb= udb(session);

    if ( udb->on_ready != NULL ){
        udb->on_ready(udb);
    }

    pthread_cond_signal(&udb->on_ready_cond);
}

/* ==================== udb_session_is_idle() ==================== */ 
int udb_session_is_idle(session_t *session)
{
    return 1;
}

/* ==================== udb_session_idle_cb() ==================== */ 
void udb_session_idle_cb(uv_idle_t *idle_handle, int status) 
{
}

/* ==================== udb_session_timer_cb() ==================== */ 
void udb_session_timer_cb(uv_timer_t *timer_handle, int status) 
{
}

/* ==================== udb_session_async_cb() ==================== */ 
void udb_session_async_cb(uv_async_t *async_handle, int status) 
{
}

/* ==================== udb_session_init() ==================== */ 
int udb_session_init(session_t *session)
{
    return 0;
}

/* ==================== udb_session_destroy() ==================== */ 
void udb_session_destroy(session_t *session) 
{
    /* FIXME */
    if ( session != NULL ){
        udb_t *udb = udb(session);
        zfree(session);
        pthread_cond_signal(&udb->main_pending_cond);
    }
}

/* ==================== udb_loop() ==================== */ 
static int udb_loop(udb_t *udb)
{
    int r;


    /* -------- server_addr -------- */
    struct sockaddr_in server_addr;
    r = uv_ip4_addr(udb->ip, udb->port, &server_addr);
    if ( r ) {
        error_log("uv_ip4_addr() failed.");
        return -1;
    }

    /* ---------- New session ---------- */
    static session_callbacks_t udb_callbacks = {
        .idle_cb = udb_session_idle_cb,
        .timer_cb = udb_session_timer_cb,
        .async_cb = udb_session_async_cb,
        .is_idle = udb_session_is_idle,
        .handle_message = udb_handle_message,
        .session_init = udb_session_init,
        .session_destroy = udb_session_destroy,

        .on_connect = NULL,

        .consume_sockbuf = NULL,
        .handle_read_response = NULL,
    };

    const session_callbacks_t *callbacks = &udb_callbacks;

    session_t *session = session_new((void*)udb, callbacks, NULL);
    udb->session = session;

    /* -------- loop -------- */
    uv_loop_t *loop = SESSION_LOOP(session);

    /* -------- tcp_handle -------- */
    uv_tcp_t *tcp_handle = SESSION_TCP(session);

    /* -------- uv_tcp_init -------- */
    r = uv_tcp_init(loop, tcp_handle);
    if ( r ) {
        error_log("uv_tcp_init() failed.");
        udb_free(udb);
        return -1;
    }
    tcp_handle->data = (void*)session; 

    /* -------- uv_tcp_connect -------- */
    uv_connect_t connect_req;
    r = uv_tcp_connect(&connect_req,
            tcp_handle,
            (const struct sockaddr*) &server_addr,
            callbacks != NULL && callbacks->on_connect != NULL ? callbacks->on_connect : on_connect);
            /*on_connect);*/
    if ( r ) {
        error_log("uv_tcp_connect() failed.");
        udb_free(udb);
        return -1;
    }

    /* -------- uv_run -------- */
    r = uv_run(loop, UV_RUN_DEFAULT);
    if ( r ) {
        error_log("uv_run() failed.");
        udb_free(udb);
        return -1;
    }

    /* FIXME */
    /*MAKE_VALGRIND_HAPPY(loop);*/

    /*close_loop(loop);      */
    /*uv_loop_delete(loop);  */


    return 0;
}

/* ==================== udb_thread_main() ==================== */ 
static void* udb_thread_main(void *arg)
{
    udb_t *udb = (udb_t*)arg;

    trace_log("Enter udb_thread_main().");

    int rc = udb_loop(udb);
    if ( rc != 0 ){
        error_log("udb_creat_session() failed.");
    }

    notice_log("udb_thread_main() %d exit.", udb->id);

    return NULL;
}

/* ==================== udb_run() ==================== */ 
int udb_run(udb_t *udb)
{
    int rc = 0;

    /*pthread_attr_t attr;*/
    /*pthread_attr_init(&attr);*/
    /*phread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);*/
    rc = pthread_create(&udb->tid, NULL, udb_thread_main, udb);

    pthread_mutex_lock(&udb->on_ready_lock);
    pthread_cond_wait(&udb->on_ready_cond, &udb->on_ready_lock);
    pthread_mutex_unlock(&udb->on_ready_lock);

    return rc;
}

/* ==================== udb_exit() ==================== */ 
void udb_exit(udb_t *udb)
{
    pthread_cancel(udb->tid);
}

