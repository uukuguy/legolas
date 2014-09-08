/**
 * @file   udclient.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-09-09 13:29:43
 * 
 * @brief  
 * 
 * 
 */

#include "udclient.h"
#include "session.h"
#include "legolas.h"

#include "zmalloc.h"
#include "adlist.h"
#include "adlist.h"
#include "uv.h"
#include "logger.h"

/* ==================== udclient_new() ==================== */ 
udclient_t *udclient_new(void *user_data)
{
    udclient_t *udcli = (udclient_t*)zmalloc(sizeof(udclient_t));
    memset(udcli, 0, sizeof(udclient_t));

    udcli->user_data = user_data;
    udcli->writing_objects = listCreate();

	pthread_mutex_init(&udcli->on_ready_lock, NULL);
	pthread_cond_init(&udcli->on_ready_cond, NULL);

    return udcli;
}


void udclient_free(udclient_t *udcli)
{
    if ( udcli != NULL ) {
        if ( udcli->writing_objects != NULL ){
            listRelease(udcli->writing_objects);
            udcli->writing_objects = NULL;
        }

        pthread_mutex_destroy(&udcli->on_ready_lock);
        pthread_cond_destroy(&udcli->on_ready_cond);

        zfree(udcli);
    }
}

/* ==================== udclient_open_data() ==================== */ 
int udclient_open_data(udclient_t *udcli, const char *key)
{
    return -1;
}

/* ==================== udclient_close_data() ==================== */ 
int udclient_close_data(udclient_t *udcli, int handle)
{
    return -1;
}

int do_write_request(session_t *session, char *data, uint32_t data_size);
/* ==================== udclient_write_data() ==================== */ 
int udclient_write_data(udclient_t *udcli, int handle, void *data, uint32_t len)
{
    session_t *session = udcli->session;

    return do_write_request(session, data, len);
}

int do_read_request(session_t *session);
/* ==================== udclient_read_data() ==================== */ 
int udclient_read_data(udclient_t *udcli, int handle, void *data, uint32_t len)
{
    session_t *session = udcli->session;
    
    return do_read_request(session);
}

int do_delete_request(session_t *session);
/* ==================== udclient_delete_data() ==================== */ 
int udclient_delete_data(udclient_t *udcli)
{

    session_t *session = udcli->session;

    return do_delete_request(session);
}

int udclient_handle_write_response(session_t *session, message_t *response);
int udclient_handle_read_response(session_t *session, message_t *response);
int udclient_handle_delete_response(session_t *session, message_t *response);

int udclient_handle_message(session_t *session, message_t *message)
{
    int ret = 0;
    udclient_t *udcli = UDCLIENT(session);

    trace_log("udclient_handle_message(). Has a message. op_code:%d", udcli->op_code);
    switch ( udcli->op_code ){
        case MSG_OP_WRITE:
            {
                if ( message->msg_type == MSG_TYPE_REQUEST ){
                } else if ( message->msg_type == MSG_TYPE_RESPONSE ) {
                    ret = udclient_handle_write_response(session, message);
                }
            } break;
        case MSG_OP_READ:
            {
                if ( message->msg_type == MSG_TYPE_REQUEST ){
                } else if ( message->msg_type == MSG_TYPE_RESPONSE ) {
                    ret = udclient_handle_read_response(session, message);
                }
            } break;
        case MSG_OP_DEL:
            {
                if ( message->msg_type == MSG_TYPE_REQUEST ){
                } else if ( message->msg_type == MSG_TYPE_RESPONSE ) {
                    ret = udclient_handle_delete_response(session, message);
                }
            } break;
    }


    return ret;
}

/* ==================== on_connect() ==================== */ 
static void on_connect(uv_connect_t *req, int status) 
{
    session_t *session = (session_t*)req->handle->data;
    UNUSED udclient_t *udcli= UDCLIENT(session);

    if ( udcli->on_ready != NULL ){
        udcli->on_ready(udcli);
    }

    pthread_cond_signal(&udcli->on_ready_cond);
}

/* ==================== udclient_session_is_idle() ==================== */ 
int udclient_session_is_idle(session_t *session)
{
    return 1;
}

/* ==================== udclient_session_idle_cb() ==================== */ 
void udclient_session_idle_cb(uv_idle_t *idle_handle, int status) 
{
}

/* ==================== udclient_session_timer_cb() ==================== */ 
void udclient_session_timer_cb(uv_timer_t *timer_handle, int status) 
{
}

/* ==================== udclient_session_async_cb() ==================== */ 
void udclient_session_async_cb(uv_async_t *async_handle, int status) 
{
}

/* ==================== udclient_session_init() ==================== */ 
int udclient_session_init(session_t *session)
{
    return 0;
}

/* ==================== udclient_session_destroy() ==================== */ 
void udclient_session_destroy(session_t *session) 
{
}

static session_callbacks_t udclient_callbacks = {
    .idle_cb = udclient_session_idle_cb,
    .timer_cb = udclient_session_timer_cb,
    .async_cb = udclient_session_async_cb,
    .is_idle = udclient_session_is_idle,
    .handle_message = udclient_handle_message,
    .session_init = udclient_session_init,
    .session_destroy = udclient_session_destroy,

    .on_connect = NULL,

    .consume_sockbuf = NULL,
    .handle_read_response = NULL,
};

/* ==================== udclient_loop() ==================== */ 
static int udclient_loop(udclient_t *udcli)
{
    int r;

    /* FIXME */
    const char *ip = "127.0.0.1";
    int port = DEFAULT_PORT;
    session_callbacks_t *callbacks = &udclient_callbacks;

    /* -------- server_addr -------- */
    struct sockaddr_in server_addr;
    r = uv_ip4_addr(ip, port, &server_addr);
    if ( r ) {
        error_log("uv_ip4_addr() failed.");
        return -1;
    }

    /* ---------- New session ---------- */
    session_t *session = session_new((void*)udcli, callbacks, NULL);
    udcli->session = session;

    /* -------- loop -------- */
    uv_loop_t *loop = SESSION_LOOP(session);

    /* -------- tcp_handle -------- */
    uv_tcp_t *tcp_handle = SESSION_TCP(session);

    /* -------- uv_tcp_init -------- */
    r = uv_tcp_init(loop, tcp_handle);
    if ( r ) {
        error_log("uv_tcp_init() failed.");
        udclient_free(udcli);
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
        udclient_free(udcli);
        return -1;
    }

    /* -------- uv_run -------- */
    r = uv_run(loop, UV_RUN_DEFAULT);
    if ( r ) {
        error_log("uv_run() failed.");
        udclient_free(udcli);
        return -1;
    }

    /* FIXME */
    /*MAKE_VALGRIND_HAPPY(loop);*/

    /*close_loop(loop);      */
    /*uv_loop_delete(loop);  */


    return 0;
}

/* ==================== udclient_thread_main() ==================== */ 
static void* udclient_thread_main(void *arg)
{
    udclient_t *udcli = (udclient_t*)arg;

    trace_log("Enter udclient_thread_main().");

    int rc = udclient_loop(udcli);
    if ( rc != 0 ){
        error_log("udclient_creat_session() failed.");
    }

    pthread_cond_signal(&udcli->on_ready_cond);

    notice_log("udclient_thread_main() %d exit.", udcli->id);

    return NULL;
}

/* ==================== udclient_run() ==================== */ 
int udclient_run(udclient_t *udcli)
{
    int rc = 0;

    /*pthread_attr_t attr;*/
    /*pthread_attr_init(&attr);*/
    /*phread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);*/
    rc = pthread_create(&udcli->tid, NULL, udclient_thread_main, udcli);

    pthread_mutex_lock(&udcli->on_ready_lock);
    pthread_cond_wait(&udcli->on_ready_cond, &udcli->on_ready_lock);
    pthread_mutex_unlock(&udcli->on_ready_lock);

    return rc;
}

/* ==================== udclient_exit() ==================== */ 
void udclient_exit(udclient_t *udcli)
{
    pthread_cancel(udcli->tid);
}

