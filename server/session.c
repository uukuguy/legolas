/**
 * @file   session.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-05-05 14:14:19
 * 
 * @brief  
 * 
 * 
 */

#include "server.h"
#include "session.h"
#include "protocol.h"
#include "uv.h"
#include "md5.h"
#include "coroutine.h"
#include <uuid/uuid.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <linux/fs.h>

static uint32_t total_sessions = 0;

/* ******************** Private Functions ******************** */
void destroy_session(session_info_t *session_info);

void empty_session_info(session_info_t *session_info);

int create_session_coroutine(session_info_t *session_info);
int destroy_session_coroutine(session_info_t *session_info);


static UNUSED void session_idle_cb(uv_idle_t *idle_handle, int status) 
{
    struct session_info_t *session_info = (struct session_info_t*)idle_handle->data;
    uint32_t finished_works = session_info->finished_works;

    uint32_t n;
    for ( n = 0 ; n < finished_works ; n++ ){
        /* -------- response message -------- */
        struct msg_response_t *response = alloc_response(0, RESULT_SUCCESS);
        uv_buf_t rbuf = uv_buf_init((char *)response, sizeof(msg_response_t));

        /* -------- write_rsp -------- */
        uv_write_t *write_rsp;
        write_rsp = zmalloc(sizeof(uv_write_t));
        write_rsp->data = session_info;

        /* -------- uv_write -------- */
        trace_log("fd(%d) Ready to call uv_write. finished_works:%d", session_fd(session_info), finished_works);

        int r = uv_write(write_rsp,
                &session_stream(session_info),
                &rbuf,
                1,
                NULL);
            /*after_response_to_client);*/

        if ( r != 0 ) {
            error_log("response failed");
        }
    }

    __sync_sub_and_fetch(&session_info->finished_works, finished_works);

}

static UNUSED void session_async_cb(uv_async_t *async_handle, int status) 
{
    /*struct session_info_t *session_info = (struct session_info_t*)async_handle->data;*/
    /*struct server_info_t *server_info = session_info->server_info;*/
    /*uv_loop_t *loop = server_info->tcp_handle.loop;*/

    /*uv_idle_t *idle_handle = (uv_idle_t *)zmalloc(sizeof(uv_idle_t));*/
    /*idle_handle->data = session_info;*/

    /*uv_idle_init(loop, idle_handle);*/
    /*uv_idle_start(idle_handle, session_idle_cb);*/
}

/* =================== on_close() ==================== */ 
/**
 * Try to destroy session_info after uv_close().
 * Called by after_shutdown().
 */
static void on_close(uv_handle_t *tcp_handle) 
{
    SESSION_INFO_FROM_UV_HANDLE(tcp_handle, session_info, server_info);

    total_sessions++;
    info_log("on_close(). total_sessions: %d", total_sessions);
    
    close_session(session_info);
}

/* ==================== after_shutdown() ==================== */ 
/**
 * Called by after_read().
 */

UNUSED static void after_shutdown(uv_shutdown_t *shutdown_req, int status) 
{
    SESSION_INFO_FROM_UV_HANDLE(shutdown_req, session_info, server_info);

    pthread_mutex_lock(&session_info->recv_pending_lock);{

        while ( session_is_waiting(session_info) == 0 ){
            pthread_cond_wait(&session_info->recv_pending_cond, 
                    &session_info->recv_pending_lock);
        }

    } pthread_mutex_unlock(&session_info->recv_pending_lock);

    session_info->waiting_for_close = 0;

    TRACE_SESSION_INFO("Do shutdown!");

    /*uv_close((uv_handle_t *)&session_info->async_handle, NULL);*/
    uv_idle_stop(&session_info->idle_handle);
    /*uv_close((uv_handle_t *)&session_info->idle_handle, NULL);*/

    uv_close((uv_handle_t*)shutdown_req->handle, on_close);

    zfree(shutdown_req);
}

/* ==================== after_read() ==================== */ 
/*
 * nread <= DEFAULT_CONN_BUF_SIZE 64 * 1024
 */

UNUSED static void after_read(uv_stream_t *handle, ssize_t nread, const uv_buf_t *buf) 
{
    SESSION_INFO_FROM_UV_HANDLE(handle, session_info, server_info);

    conn_buf_t *cob = container_of(buf->base, conn_buf_t, base);
    assert(session_info == cob->session_info);

    if ( nread > 0 ) {
        /* -------------------------------------------------------------------- */
        /* Normal handle. */

        cob->write_head += nread;
        session_info->connection.total_bytes += nread;
        __sync_add_and_fetch(&cob->remain_bytes, nread);

        trace_log("\n........\nfd(%d) block(%d) nread=%zu bytes. write_head:%d, remain_bytes=%d, total_bytes=%d\n", session_fd(session_info), cob->blockid, nread, cob->write_head, cob->remain_bytes, session_info->connection.total_bytes);

        enqueue_recv_queue(session_info, cob);
        pthread_yield();

        /* FIXME */
        while ( server_info->cached_bytes > MAX_CACHED_BYTES ) {
            usleep(10 * 1000);
            pthread_yield();
        }


    } else if (nread < 0) {
        /* -------------------------------------------------------------------- */
        /* Error or EOF. Must shutdown. */

        delete_cob(cob);

        /* -------- UV__ENOBUFS -------- */
        if ( nread == UV__ENOBUFS ) {
            trace_log("It's UV__ENOBUFS");
            return;
        }

        /* -------- UV__EOF -------- */
        if ( nread == UV__EOF ) {
            /*info_log("It's UV__EOF");*/
        } else {
            warning_log("read error. errno=-%d total_bytes=%d", ~(uint32_t)(nread - 1), session_info->connection.total_bytes);
        }

        /* -------- Shutdown -------- */
        session_info->waiting_for_close = 1;
        uv_shutdown_t* shutdown_req = (uv_shutdown_t*)zmalloc(sizeof(uv_shutdown_t));
        shutdown_req->data = session_info;
        uv_shutdown(shutdown_req, handle, after_shutdown);

        return;
    } else { 
        /* -------------------------------------------------------------------- */
        /* nread == 0 Everything OK, but nothing read. */

        delete_cob(cob);
        return;
    }

}

/* ==================== session_alloc() ==================== */ 

UNUSED static void session_alloc(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) 
{
    SESSION_INFO_FROM_UV_HANDLE(handle, session_info, server_info);

    /* -------- cob -------- */
    uint32_t buf_len = sizeof(conn_buf_t);
    struct conn_buf_t *cob = (struct conn_buf_t *)zmalloc(buf_len);
    cob->session_info = session_info;
    init_cob(cob);
    __sync_add_and_fetch(&session_info->cached_bytes, buf_len);
    __sync_add_and_fetch(&server_info->cached_bytes, buf_len);

    /* XXX Calculate total blocks for debug. */
    cob->blockid = __sync_add_and_fetch(&session_info->total_blocks, 1);

    /* -------- set uv_buf -------- */
    buf->base = cob->base;
    buf->len = cob->len;

}

/* ==================== create_session() ==================== */ 
session_info_t* create_session(server_info_t *server_info)
{
    static uint32_t seq_no = 0;

    /* -------- session_info -------- */
    session_info_t *session_info = (session_info_t*)zmalloc(sizeof(session_info_t));
    memset(session_info, 0, sizeof(session_info_t));
    session_info->server_info = server_info;
	session_info->sid.seq_no = seq_no++;
	pthread_mutex_init(&session_info->recv_pending_lock, NULL);
	pthread_cond_init(&session_info->recv_pending_cond, NULL);
	pthread_mutex_init(&session_info->send_pending_lock, NULL);
	pthread_cond_init(&session_info->send_pending_cond, NULL);
	pthread_mutex_init(&session_info->after_response_lock, NULL);
	pthread_cond_init(&session_info->after_response_cond, NULL);

    empty_session_info(session_info);

    if ( create_session_coroutine(session_info) != 0 ) {
        destroy_session(session_info);
        return NULL;
    }

    int ret;

    /* -------- loop -------- */
    uv_loop_t *loop =  server_info->tcp_handle.loop;

    /*session_info->async_handle.data = session_info;*/
    /*uv_async_init(loop, &session_info->async_handle, session_async_cb);*/

    uv_idle_t *idle_handle = &session_info->idle_handle;
    idle_handle->data = session_info;
    uv_idle_init(loop, idle_handle);
    uv_idle_start(idle_handle, session_idle_cb);

    /* -------- tcp_handle -------- */
    uv_tcp_t *tcp_handle = &session_info->connection.handle.tcp;
    tcp_handle->data = session_info;

    /* -------- uv_tcp_init -------- */
    ret = uv_tcp_init(loop, tcp_handle);
    if ( ret != 0 ) { 
        destroy_session(session_info);
        error_log("uv_tcp_init() failed. ret = %d", ret); 
        return NULL; 
    }

    /* -------- uv_accept -------- */
    ret = uv_accept((uv_stream_t*)&server_info->tcp_handle, (uv_stream_t*)tcp_handle);
    if ( ret != 0 ) { 
        destroy_session(session_info);
        error_log("uv_accept() failed. ret = %d", ret); 
        return NULL; 
    }


    /* -------- session_rx_on -------- */
    ret = session_rx_on(session_info);
    if ( ret != 0 ) { 
        destroy_session(session_info);
        error_log("session_rx_on() failed. ret = %d", ret); 
        return NULL; 
    }

    return session_info;
}

/* ************************************************************
 *
 *                 Session Private Functions
 *
 * ************************************************************/

/* ==================== destroy_session() ==================== */ 
void destroy_session(session_info_t *session_info)
{
    assert(session_info != NULL);

    destroy_session_coroutine(session_info);


    pthread_mutex_destroy(&session_info->recv_pending_lock);
    pthread_cond_destroy(&session_info->recv_pending_cond);
    pthread_mutex_destroy(&session_info->send_pending_lock);
    pthread_cond_destroy(&session_info->send_pending_cond);
    pthread_mutex_destroy(&session_info->after_response_lock);
    pthread_cond_destroy(&session_info->after_response_cond);

    zfree(session_info);
}

/* ==================== close_session() ==================== */ 
void close_session(session_info_t *session_info)
{
    destroy_session(session_info);
}

/* ==================== session_incref() ==================== */ 
UNUSED void session_incref(struct session_info_t *session_info)
{
	if ( session_info != NULL ){
		__sync_add_and_fetch(&session_info->refcnt, 1);
    }
}

/* ==================== session_decref() ==================== */ 
UNUSED void session_decref(struct session_info_t *session_info)
{
	if ( session_info != NULL ) {
		int refcnt = __sync_sub_and_fetch(&session_info->refcnt, 1);
        if ( refcnt == 0 ) {
            close_session(session_info);
        }
	}
}

/* ==================== init_cob() ==================== */ 
void init_cob(conn_buf_t *cob)
{
    assert(cob != NULL );

    cob->read_head = 0;
    cob->read_tail = 0;
    cob->read_eob = 0;
    cob->write_head = 0;
    cob->write_tail = 0;
    cob->len = DEFAULT_CONN_BUF_SIZE;
    cob->remain_bytes = 0;
    cob->least_size = sizeof(msg_request_t);
    INIT_LIST_HEAD(&cob->rx_block_list);
}

/* ==================== delete_cob() ==================== */ 
void delete_cob(conn_buf_t *cob)
{
    session_info_t *session_info = cob->session_info;
    server_info_t *server_info = session_info->server_info;
    zfree(cob);
    __sync_sub_and_fetch(&session_info->cached_bytes, sizeof(conn_buf_t));
    __sync_sub_and_fetch(&server_info->cached_bytes, sizeof(conn_buf_t));
}

/* ==================== empty_session_info() ==================== */ 
void empty_session_info(session_info_t *session_info)
{
    session_info->session_status = SESSION_STATUS_HEAD;
    session_info->f = NULL;
    session_info->refcnt = 1;
    session_info->waiting_for_close = 0;
    session_info->total_blocks = 0;
    session_info->total_received_buffers = 0;
    session_info->total_saved_buffers = 0;
	session_info->sid.nodeid = 0;
    INIT_LIST_HEAD(&session_info->rx_block_queue);

    session_info->connection.session_info = session_info;
    session_info->connection.total_bytes = 0;
    session_info->cached_bytes = 0;
    session_info->finished_works = 0;

    /* Init conn_buf_t field in session_info->connection.*/
    session_info->connection.cob.session_info = session_info;
    init_cob(&session_info->connection.cob);
}

extern void *session_rx_handler(void *opaque);
extern void *session_tx_handler(void *opaque);
/* ==================== create_session_coroutine() ==================== */ 
int create_session_coroutine(session_info_t *session_info)
{
    session_info->rx_co = coroutine_create(session_rx_handler);
    if ( session_info->rx_co == NULL ) {
        error_log("Cann't create coroutine session_info->rx_co");
        return -1;
    }

    session_info->tx_co = coroutine_create(session_tx_handler);
    if ( session_info->tx_co == NULL ) {
        error_log("Cann't create coroutine session_info->tx_co");
        return -1;
    }

    return 0;
}

/* ==================== destroy_session_coroutine() ==================== */ 
int destroy_session_coroutine(session_info_t *session_info)
{
    if ( session_info->rx_co != NULL ){
        coroutine_delete(session_info->rx_co);
        session_info->rx_co = NULL;
    }

    if ( session_info->tx_co != NULL ){
        coroutine_delete(session_info->tx_co);
        session_info->tx_co = NULL;
    }

    return 0;
}

/* ************************************************************
 *
 *                      Private Functions
 *
 * ************************************************************/

/* ==================== too_many_requests() ==================== */ 
int too_many_requests(session_info_t *session_info)
{
    int ret = 0;
    /*ret = session_info->outstanding_reqs > 10000 || */
        /*session_info->connection.total_bytes > 4 * 1048576 ||*/
    return ret;
}

void session_finish_saving_buffer(session_info_t *session_info){
    pthread_mutex_lock(&session_info->recv_pending_lock);{
        session_info->total_saved_buffers++;
    } pthread_mutex_unlock(&session_info->recv_pending_lock);
}

/* ==================== session_is_waiting() ==================== */ 
int session_is_waiting(session_info_t *session_info)
{
    assert(session_info->total_received_buffers >= session_info->total_saved_buffers);

    int ret = 0;

    if ( session_info->total_received_buffers == 
            session_info->total_saved_buffers ){
        ret = 1;
    } else {
        ret = 0;
    }

    return ret;
}

/* ==================== session_rx_on() ==================== */ 
int session_rx_on(session_info_t *session_info)
{
    return uv_read_start((uv_stream_t*)&session_stream(session_info), session_alloc, after_read);
}

/* ==================== session_rx_off() ==================== */ 
void session_rx_off(session_info_t *session_info)
{
    uv_read_stop((uv_stream_t*)&session_stream(session_info));
}

/* ==================== session_tx_on() ==================== */ 
int session_tx_on(session_info_t *session_info)
{
    return 0;
}

/* ==================== session_tx_off() ==================== */ 
void session_tx_off(session_info_t *session_info)
{
}

/* ==================== remove_from_recv_queue() ==================== */ 
void remove_from_recv_queue(session_info_t *session_info, conn_buf_t *cob) 
{
    /*session_info_t *session_info = cob->session_info;*/

    int fd = session_fd(session_info);

    server_info_t *server_info = session_info->server_info;

    struct work_queue *wq = server_info->recv_queue[fd % RECV_THREADS];

    if ( wq != NULL ) {
        remove_work(wq, &cob->rx_block_list);
    }
}

/* ==================== enqueue_recv_queue() ==================== */ 
void enqueue_recv_queue(session_info_t *session_info, conn_buf_t *cob)
{
    pthread_mutex_lock(&session_info->recv_pending_lock);{
        session_info->total_received_buffers++;
    } pthread_mutex_unlock(&session_info->recv_pending_lock);

    int fd = session_fd(session_info);    

    struct work_queue *wq = session_info->server_info->recv_queue[fd % RECV_THREADS];

    if ( wq != NULL ) {
        enqueue_work(wq, (void*)cob);
    }
}

/* ==================== dequeue_recv_queue() ==================== */ 
conn_buf_t *dequeue_recv_queue(session_info_t *session_info)
{
    int fd = session_fd(session_info);    

    struct work_queue *wq = session_info->server_info->recv_queue[fd % RECV_THREADS];

    if ( wq != NULL ) {
        return dequeue_work(wq);
    }

    return NULL;
}

