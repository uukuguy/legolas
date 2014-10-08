/**
 * @file   session.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-05-05 14:14:19
 * 
 * @brief  
 * 
 * 
 */

#include "session.h"
#include "session.h"
#include "message.h"
#include "uv.h"
#include "md5.h"
/*#include "coroutine.h"*/
#include "crc32.h"
#include "byteblock.h"
#include "react_utils.h"
#include <uuid/uuid.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#ifdef OS_LINUX
#include <linux/fs.h>
#endif

static uint32_t total_sessions = 0;

/* in session_parsing_message.c */
void session_alloc(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf);
void after_read(uv_stream_t *handle, ssize_t nread, const uv_buf_t *buf);

int connection_init(connection_t *connection)
{
    connection->total_bytes = 0;
    memset(&connection->loop, 0, sizeof(uv_loop_t));
    uv_loop_init(&connection->loop);

    return 0;
}

void connection_destroy(connection_t *connection)
{
    /*uv_loop_close(&connection->loop);*/
}

/* ******************** Private Functions ******************** */
static void after_session_send_data(uv_write_t *write_rsp, int status) 
{
    zfree(write_rsp);
}

int session_send_data(session_t *session, char *buf, uint32_t buf_size, void *user_data, uv_write_cb after_write)
{
    int ret = 0;

    /* -------- response message -------- */
    uv_buf_t rbuf = uv_buf_init(buf, buf_size);

    /* -------- write_rsp -------- */
    uv_write_t *write_rsp;
    write_rsp = zmalloc(sizeof(uv_write_t));
    memset(write_rsp, 0, sizeof(uv_write_t));
    write_rsp->data = user_data;

    /* -------- uv_write -------- */
    ret = uv_write(write_rsp,
            &session_stream(session),
            &rbuf,
            1,
            after_write != NULL ? after_write : after_session_send_data);

    if ( ret != 0 ) {
        error_log("response failed");
    }

    return ret;
}

int session_response_data(session_t *session, char *buf, uint32_t buf_size)
{
    int ret;

    ret = session_send_data(session, buf, buf_size, (void*)session, NULL);

    return ret;
}

/* ==================== session_response() ==================== */ 
void session_response(session_t *session, enum MSG_RESULT result)
{
    message_t *response = alloc_response_message(result);

    uint32_t msg_size = sizeof(message_t) + response->data_length;
    session_response_data(session, (char *)response, msg_size);

    zfree(response);
}

/* =================== on_close() ==================== */ 
/**
 * Try to destroy session after uv_close().
 * Called by after_shutdown().
 */
    /* FIXME */
UNUSED static void on_close(uv_handle_t *tcp_handle) 
{
    session_t *session = (session_t *)tcp_handle->data; 

    total_sessions++;
    info_log("on_close(). session(%d) total_sessions: %d total_finished_works: %d", session->id, total_sessions, session->total_finished_works);
    
    session_free(session);
}

/* ==================== after_shutdown() ==================== */ 
/**
 * Called by after_read().
 */

void after_shutdown(uv_shutdown_t *shutdown_req, int status) 
{
    session_t *session = (session_t *)shutdown_req->data; 

    /*pthread_mutex_lock(&session->recv_pending_lock);{*/

        /*while ( session->is_idle(session) == 0 ){*/
            /*pthread_cond_wait(&session->recv_pending_cond, */
                    /*&session->recv_pending_lock);*/
        /*}*/

    /*} pthread_mutex_unlock(&session->recv_pending_lock);*/

    session->waiting_for_close = 0;

    TRACE_session("Do shutdown!");

    /*if ( session->callbacks.async_cb != NULL ) {*/
        /*uv_close((uv_handle_t *)&session->async_handle, NULL);*/
    /*}*/

    if ( session->callbacks.timer_cb != NULL ){
        uv_timer_stop(&session->timer_handle);
    }


    if ( session->callbacks.idle_cb != NULL ){
        uv_idle_stop(&session->idle_handle);
    }


    /* FIXME */
    uv_close((uv_handle_t*)shutdown_req->handle, on_close);
    /*uv_close((uv_handle_t*)shutdown_req->handle, NULL);*/

    zfree(shutdown_req);
}

void session_shutdown(session_t *session)
{
    if ( !session->stop ){

        if ( session->waiting_for_close == 1 ) {
            int is_idle = 1;
            if ( session->callbacks.is_idle != NULL &&
                    session->callbacks.is_idle(session) != 1 ) 
                is_idle = 0;
            if ( is_idle == 1 ) {
            /*if ( session->callbacks.is_idle(session)) {*/
                session->stop = 1;

                info_log("Start to close session in session_shutdown()");
                uv_timer_stop(&session->timer_handle);


                uv_shutdown_t* shutdown_req = (uv_shutdown_t*)zmalloc(sizeof(uv_shutdown_t));
                memset(shutdown_req, 0, sizeof(uv_shutdown_t));
                shutdown_req->data = session;
                uv_shutdown(shutdown_req, &session->connection.handle.stream, after_shutdown);
                return;
            }
        }
    }
}

/* FIXME Only for server now. */
int create_session_coroutine(session_t *session);
int destroy_session_coroutine(session_t *session);

static uint32_t session_id = 0;
/* ==================== session_new() ==================== */ 
session_t* session_new(service_t *service, const session_callbacks_t *callbacks, void *user_data)
{

    /* -------- session -------- */
    session_t *session = (session_t*)zmalloc(sizeof(session_t));
    memset(session, 0, sizeof(session_t));

    char react_filename[NAME_MAX];
    sprintf(react_filename, "./session_%d.json", session_id);
    session->react_ctx = REACT_INIT(react_filename);

    session->react_id_session = react_define_new_action("session"); 
    react_start_action(session->react_id_session);

    session->service = service;

    session->id = session_id;
    __sync_add_and_fetch(&session_id, 1);

    if ( callbacks != NULL )
        session->callbacks = *callbacks;
	pthread_mutex_init(&session->recv_pending_lock, NULL);
	pthread_cond_init(&session->recv_pending_cond, NULL);
	pthread_mutex_init(&session->send_pending_lock, NULL);
	pthread_cond_init(&session->send_pending_cond, NULL);

    session->stop = 0;
    session->waiting_for_close = 0;
    session->total_blocks = 0;
    session->total_received_buffers = 0;
    session->total_saved_buffers = 0;
	/*session->sid.nodeid = 0;*/

    connection_init(&session->connection);

    session->total_readed = 0;
    session->total_writed = 0;
    session->cached_bytes = 0;
    session->finished_works = 0;
    session->total_finished_works = 0;
    session->user_data = user_data;

    if ( create_session_coroutine(session) != 0 ) {
        error_log("create_session_coroutine() failed.");
        session_free(session);
        return NULL; 
    }
    session->responseQueue = listCreate();
    listSetFreeMethod(session->responseQueue, byte_block_free);

    if ( callbacks->session_init != NULL ) {
        if ( callbacks->session_init(session) != 0 ) {
            session_free(session);
            return NULL;
        }
    }

    return session;
}

/* ==================== session_accept() ==================== */ 
int session_accept(session_t *session, uv_tcp_t *parent_tcp)
{
    REACT_ACTION_START(session_accept);

    int ret = 0;

    /* -------- tcp_handle -------- */
    uv_tcp_t *tcp_handle = SESSION_TCP(session);
    tcp_handle->data = session;

    uv_loop_t *loop = parent_tcp->loop;

    session_callbacks_t *callbacks = &session->callbacks;

    if ( callbacks->async_cb != NULL ) {
        uv_async_init(loop, &session->async_handle, callbacks->async_cb);
        session->async_handle.data = session;
    }

    if ( callbacks->idle_cb != NULL ) {
        uv_idle_t *idle_handle = &session->idle_handle;
        idle_handle->data = session;
        uv_idle_init(loop, idle_handle);
        uv_idle_start(idle_handle, callbacks->idle_cb);
    }

    if ( callbacks->timer_cb != NULL ) {
        uv_timer_init(loop, &session->timer_handle);
        session->timer_handle.data = session;
        /*session_timer_cb*/
        uv_timer_start(&session->timer_handle, callbacks->timer_cb, 1000, 10000);
    }

    /* -------- uv_tcp_init -------- */
    ret = uv_tcp_init(loop, tcp_handle);
    if ( ret != 0 ) { 
        session_free(session);
        error_log("uv_tcp_init() failed. ret = %d", ret); 
        return -1; 
    }

    /* -------- uv_accept -------- */
    ret = uv_accept((uv_stream_t*)parent_tcp, (uv_stream_t*)tcp_handle);
    if ( ret != 0 ) { 
        session_free(session);
        error_log("uv_accept() failed. ret = %d", ret); 
        return -1; 
    }


    /* -------- session_waiting_message -------- */
    ret = session_waiting_message(session);
    if ( ret != 0 ) { 
        session_free(session);
        error_log("session_waiting_message() failed. ret = %d", ret); 
        return -1; 
    }

    REACT_ACTION_STOP(session_accept);

    return ret;
}

/* ************************************************************
 *
 *                 Session Private Functions
 *
 * ************************************************************/

/* ==================== session_free() ==================== */ 
void session_free(session_t *session)
{
    notice_log("session_free() session.id: %d", session->id);
    assert(session != NULL);

    destroy_session_coroutine(session);
    listRelease(session->responseQueue);

    connection_destroy(&session->connection);

    if ( session->user_data != NULL ) {
        zfree(session->user_data);
        session->user_data = NULL;
    }

    pthread_mutex_destroy(&session->recv_pending_lock);
    pthread_cond_destroy(&session->recv_pending_cond);
    pthread_mutex_destroy(&session->send_pending_lock);
    pthread_cond_destroy(&session->send_pending_cond);


    react_stop_action(session->react_id_session);

    REACT_CLEANUP(session->react_ctx);

    if ( session->callbacks.session_destroy != NULL ) {
        session->callbacks.session_destroy(session);
    } else {
        zfree(session);
    }
}

/* ==================== sockbuf_init() ==================== */ 
void sockbuf_init(sockbuf_t *sockbuf)
{
    assert(sockbuf != NULL );

    sockbuf->len = DEFAULT_SOCKBUF_SIZE;
    sockbuf->read_head = 0;
    sockbuf->write_head = 0;
    sockbuf->remain_bytes = 0;
    sockbuf->blockid = 0;
    /*sockbuf->read_tail = 0;*/
    /*sockbuf->read_eob = 0;*/
    /*sockbuf->write_tail = 0;*/
    /*sockbuf->least_size = sizeof(message_t);*/
}

sockbuf_t *sockbuf_new(session_t *session)
{
    sockbuf_t *sockbuf = (sockbuf_t *)zmalloc(sizeof(sockbuf_t));
    memset(sockbuf, 0, sizeof(sockbuf_t));
    sockbuf->session = session;
    sockbuf_init(sockbuf);
    return sockbuf;
}

/* ==================== delete_sockbuf() ==================== */ 
void sockbuf_free(sockbuf_t *sockbuf)
{
    session_t *session = sockbuf->session;
    if ( session != NULL ){
        __sync_sub_and_fetch(&session->cached_bytes, sizeof(sockbuf_t));
        /*server_t *server = session->server;*/
        /*if ( server != NULL ){*/
            /*__sync_sub_and_fetch(&server->cached_bytes, sizeof(sockbuf_t));*/
        /*}*/
    }

    zfree(sockbuf);
}


/* ************************************************************
 *
 *                      Private Functions
 *
 * ************************************************************/

/* ==================== too_many_requests() ==================== */ 
int too_many_requests(session_t *session)
{
    int ret = 0;
    /*ret = session->outstanding_reqs > 10000 || */
        /*session->connection.total_bytes > 4 * 1048576 ||*/
    return ret;
}

/* ==================== session_rx_on() ==================== */ 
int session_rx_on(session_t *session)
{
    return uv_read_start((uv_stream_t*)&session_stream(session), session_alloc, after_read);
}

/* ==================== session_rx_off() ==================== */ 
void session_rx_off(session_t *session)
{
    uv_read_stop((uv_stream_t*)&session_stream(session));
}

