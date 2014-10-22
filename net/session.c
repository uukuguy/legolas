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
#include "service.h"
#include "message.h"
#include "uv.h"
#include "md5.h"
/*#include "coroutine.h"*/
#include "crc32.h"
#include "byteblock.h"
/*#include "react_utils.h"*/
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
void session_after_read(uv_stream_t *handle, ssize_t nread, const uv_buf_t *buf);

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
    /*pthread_mutex_lock(&session->send_pending_lock);*/

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


    /*pthread_mutex_unlock(&session->send_pending_lock);*/

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
 * Called by session_after_shutdown().
 */
    /* FIXME */
UNUSED static void on_close(uv_handle_t *tcp_handle) 
{
    session_t *session = (session_t *)tcp_handle->data; 

    total_sessions++;
    info_log("on_close(). session(%d) total_sessions: %d total_finished_works: %d", session->id, total_sessions, session->total_finished_works);
    
    session_free(session);
}

/* ==================== session_after_shutdown() ==================== */ 
/**
 * Called by session_after_read().
 */

void session_after_shutdown(uv_shutdown_t *shutdown_req, int status) 
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

    /*if ( session->service->callbacks.async_cb != NULL ) {*/
        /*uv_close((uv_handle_t *)&session->async_handle, NULL);*/
    /*}*/

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
            if ( session->service->callbacks.is_idle != NULL &&
                    session->service->callbacks.is_idle(session) != 1 ) 
                is_idle = 0;
            if ( is_idle == 1 ) {
            /*if ( session->service->callbacks.is_idle(session)) {*/
                session->stop = 1;

                info_log("Start to close session in session_shutdown()");

                if ( session->service->callbacks.timer_cb != NULL ){
                    uv_timer_stop(&session->timer_handle);
                }

                if ( session->service->callbacks.idle_cb != NULL ){
                    uv_idle_stop(&session->idle_handle);
                }

                uv_shutdown_t* shutdown_req = (uv_shutdown_t*)zmalloc(sizeof(uv_shutdown_t));
                memset(shutdown_req, 0, sizeof(uv_shutdown_t));
                shutdown_req->data = session;
                uv_shutdown(shutdown_req, &session->connection.handle.stream, session_after_shutdown);
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
session_t* session_new(service_t *service, void *user_data)
{
    assert(service != NULL);

    /* -------- session -------- */
    session_t *session = (session_t*)zmalloc(sizeof(session_t));
    memset(session, 0, sizeof(session_t));

    session->msgctx.current_state = ConsumeState_WAITING_HEAD;
    session->msgctx.except_size = sizeof(message_t);
    session->msgctx.consumed_size = 0;

    /*char react_filename[NAME_MAX];*/
    /*sprintf(react_filename, "./session_%d.json", session_id);*/
    /*session->react_ctx = REACT_INIT(react_filename);*/

    /*session->react_id_session = react_define_new_action("session"); */
    /*react_start_action(session->react_id_session);*/

    session->service = service;

    session->id = session_id;
    __sync_add_and_fetch(&session_id, 1);

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

    const session_callbacks_t *callbacks = &service->callbacks;
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
    /*REACT_ACTION_START(session_accept);*/

    int ret = 0;

    /* -------- tcp_handle -------- */
    uv_tcp_t *tcp_handle = SESSION_TCP(session);
    tcp_handle->data = session;

    uv_loop_t *loop = parent_tcp->loop;

    session_callbacks_t *callbacks = &session->service->callbacks;

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

    /*REACT_ACTION_STOP(session_accept);*/

    return ret;
}

/* ==================== session_on_connect_to_server() ==================== */ 
static void session_on_connect_to_server(uv_connect_t *req, int status) 
{
    /*service_t *service = (service_t*)req->handle->data;*/
    /*session_callbacks_t *callbacks = &service->callbacks;*/
    session_t *session = (session_t*)req->handle->data;
    session_callbacks_t *callbacks = &session->service->callbacks;
    if ( callbacks->session_on_connect_to_server != NULL ){
        /*callbacks->session_on_connect_to_server(service, status);*/
        callbacks->session_on_connect_to_server(session, status);
    }
    
}

/* ==================== session_on_connect_from_client() ==================== */ 
static void session_on_connect_from_client(uv_stream_t *stream, int status) 
{
    service_t *service = (service_t*)stream->data;
    session_callbacks_t *callbacks = &service->callbacks;
    if ( callbacks->session_on_connect_from_client != NULL ){
        callbacks->session_on_connect_from_client(service, status);
    }
    
}

/* ==================== session_listen() ==================== */ 
int session_listen(service_t *service, int listen_port) 
{
    int r;

    /* -------- server_addr -------- */
    struct sockaddr_in server_addr;
    r = uv_ip4_addr("0.0.0.0", listen_port, &server_addr);
    if ( r ) {
        error_log("uv_ip4_addr() failed.");
        return -1;
    }

    /* -------- loop -------- */
    uv_loop_t *loop = &service->connection.loop;

    /* -------- tcp_handle -------- */
    uv_tcp_t *tcp_handle = &service->connection.handle.tcp;

    /* -------- uv_tcp_init -------- */
    r = uv_tcp_init(loop, tcp_handle);
    if ( r ) {
        error_log("uv_tcp_init() failed.");
        return -1;
    }
    tcp_handle->data = service;

    /* -------- uv_tcp_bind -------- */
    r = uv_tcp_bind(tcp_handle, (const struct sockaddr*)&server_addr, 0);
    if ( r ) {
        error_log("uv_tcp_bind() failed.");
        return -1;
    }

    /* -------- uv_listen -------- */
    r = uv_listen((uv_stream_t*)tcp_handle, SOMAXCONN, session_on_connect_from_client);
    if ( r ) {
        error_log("uv_listen() failed.");
        return -1;
    }

    info_log("Listen on port %d.", listen_port);

    /* -------- uv_run -------- */
    r = uv_run(loop, UV_RUN_DEFAULT);
    if ( r ) {
        error_log("uv_run() failed.");
        return -1;
    }

    /* FIXME */
    /*MAKE_VALGRIND_HAPPY(loop);*/

    /*close_loop(loop);      */
    /*uv_loop_delete(loop);  */

    return r;
}

/* ==================== session_loop() ==================== */ 
int session_loop(session_t *session, const char *ip, int port)
{
    int r;


    /* -------- server_addr -------- */
    struct sockaddr_in server_addr;
    r = uv_ip4_addr(ip, port, &server_addr);
    if ( r ) {
        error_log("uv_ip4_addr() failed.");
        return -1;
    }

    /* -------- loop -------- */
    uv_loop_t *loop = SESSION_LOOP(session);

    /* -------- tcp_handle -------- */
    uv_tcp_t *tcp_handle = SESSION_TCP(session);

    /* -------- uv_tcp_init -------- */
    r = uv_tcp_init(loop, tcp_handle);
    if ( r ) {
        error_log("uv_tcp_init() failed.");
        return -1;
    }

    tcp_handle->data = (void*)session; 
    /*tcp_handle->data = (void*)session->service; */

    /* -------- uv_tcp_connect -------- */
    uv_connect_t connect_req;
    r = uv_tcp_connect(&connect_req,
            tcp_handle,
            (const struct sockaddr*) &server_addr,
            session_on_connect_to_server);
    if ( r ) {
        error_log("uv_tcp_connect() failed.");
        return -1;
    }

    /* -------- uv_run -------- */
    r = uv_run(loop, UV_RUN_DEFAULT);
    if ( r ) {
        error_log("uv_run() failed.");
        return -1;
    }

    /* FIXME */
    /*MAKE_VALGRIND_HAPPY(loop);*/

    /*close_loop(loop);      */
    /*uv_loop_delete(loop);  */


    return 0;
}

/* ==================== session_waiting_message() ==================== */ 
int session_waiting_message(session_t *session)
{
    return uv_read_start((uv_stream_t*)&session_stream(session), session_alloc, session_after_read);
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


    /*react_stop_action(session->react_id_session);*/

    /*REACT_CLEANUP(session->react_ctx);*/

    if ( session->service->callbacks.session_destroy != NULL ) {
        session->service->callbacks.session_destroy(session);
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
    return uv_read_start((uv_stream_t*)&session_stream(session), session_alloc, session_after_read);
}

/* ==================== session_rx_off() ==================== */ 
void session_rx_off(session_t *session)
{
    uv_read_stop((uv_stream_t*)&session_stream(session));
}


static void session_after_write_request(uv_write_t *write_req, int status) 
{
    UNUSED session_t *session = (session_t*)write_req->data;
    if ( session->after_write_request != NULL ){
        session->after_write_request(session, status);
    }
    zfree(write_req);
}

/*int session_write_request(session_t *session, void *data, uint32_t data_size, uv_write_cb after_write)*/
int session_write_request(session_t *session, void *data, uint32_t data_size, session_after_write_request_cb after_write_request)
{
    uv_buf_t ubuf = uv_buf_init(data, data_size);

    /* -------- write_req -------- */
    uv_write_t *write_req;
    write_req = zmalloc(sizeof(uv_write_t));
    memset(write_req, 0, sizeof(uv_write_t));
    write_req->data = session;

    session->after_write_request = after_write_request;
    int r = uv_write(write_req,
            &session->connection.handle.stream,
            &ubuf,
            1,
            session_after_write_request);

    if ( r != 0 ) {
        error_log("uv_write() failed");
        return -1;
    }

    return r;
}

