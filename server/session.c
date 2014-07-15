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
#include "crc32.h"
#include "byteblock.h"
#include <uuid/uuid.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <linux/fs.h>

static uint32_t total_sessions = 0;

/* ******************** Private Functions ******************** */
void destroy_session(session_t *session);

void empty_session(session_t *session);

int create_session_coroutine(session_t *session);
int destroy_session_coroutine(session_t *session);

int session_handle_request(session_t *session, msg_request_t *request);

UNUSED static void after_response_to_client(uv_write_t *write_rsp, int status) 
{
    zfree(write_rsp);
}

int session_response(session_t *session, char *buf, uint32_t buf_size)
{
    byte_block_t *bb = byte_block_attach(buf, buf_size);

    pthread_mutex_lock(&session->send_pending_lock);
    listAddNodeTail(session->responseQueue, bb);
    pthread_mutex_unlock(&session->send_pending_lock);

    return 0;
}

int session_send_data(session_t *session, char *buf, uint32_t buf_size, uv_write_cb after_write)
{
    int ret = 0;
    /*server_t *server = session->server;*/

    /* -------- response message -------- */
    uv_buf_t rbuf = uv_buf_init(buf, buf_size);

    /* -------- write_rsp -------- */
    uv_write_t *write_rsp;
    write_rsp = zmalloc(sizeof(uv_write_t));
    write_rsp->data = session;

    /*session_rx_off(session);*/
    /*pthread_mutex_lock(&session->send_pending_lock);*/
    /*pthread_mutex_lock(&server->send_pending_lock);*/
    /* -------- uv_write -------- */
    ret = uv_write(write_rsp,
            &session_stream(session),
            &rbuf,
            1,
            after_write);


    /*pthread_mutex_unlock(&session->send_pending_lock);*/
    /*pthread_mutex_unlock(&server->send_pending_lock);*/
    /*session_rx_on(session);*/

    if ( ret != 0 ) {
        error_log("response failed");
    }

    return ret;
}

/* =================== on_close() ==================== */ 
/**
 * Try to destroy session after uv_close().
 * Called by after_shutdown().
 */
static void on_close(uv_handle_t *tcp_handle) 
{
    session_FROM_UV_HANDLE(tcp_handle);

    total_sessions++;
    info_log("on_close(). total_sessions: %d", total_sessions);
    
    close_session(session);
}

/* ==================== after_shutdown() ==================== */ 
/**
 * Called by after_read().
 */

UNUSED static void after_shutdown(uv_shutdown_t *shutdown_req, int status) 
{
    session_FROM_UV_HANDLE(shutdown_req);

    /*pthread_mutex_lock(&session->recv_pending_lock);{*/

        /*while ( session_is_waiting(session) == 0 ){*/
            /*pthread_cond_wait(&session->recv_pending_cond, */
                    /*&session->recv_pending_lock);*/
        /*}*/

    /*} pthread_mutex_unlock(&session->recv_pending_lock);*/

    session->waiting_for_close = 0;

    TRACE_session("Do shutdown!");

    uv_timer_stop(&session->timer_handle);

    /*uv_close((uv_handle_t *)&session->async_handle, NULL);*/
    uv_idle_stop(&session->idle_handle);
    /*uv_close((uv_handle_t *)&session->idle_handle, NULL);*/

    uv_close((uv_handle_t*)shutdown_req->handle, on_close);

    zfree(shutdown_req);
}

static UNUSED void session_idle_cb(uv_idle_t *idle_handle, int status) 
{
    session_t *session = (session_t*)idle_handle->data;

    if ( session->waiting_for_close == 1 ) {
        if ( session_is_waiting(session)) {
            info_log("Start to close session in session_timer_cb()");
            uv_timer_stop(&session->timer_handle);

            uv_shutdown_t* shutdown_req = (uv_shutdown_t*)zmalloc(sizeof(uv_shutdown_t));
            shutdown_req->data = session;
            uv_shutdown(shutdown_req, &session->connection.handle.stream, after_shutdown);
            return;
        }
    }

    uint32_t nResponses = listLength(session->responseQueue);
    while ( nResponses-- > 0 ){
        
        pthread_mutex_lock(&session->send_pending_lock);
        listNode *first = listFirst(session->responseQueue);
        void *nodeData = listNodeValue(first);
        byte_block_t *bb = (byte_block_t*)nodeData;
        session_send_data(session, bb->buf, bb->size, NULL);
        listDelNode(session->responseQueue, first);
        pthread_mutex_unlock(&session->send_pending_lock);
    };

    /*uint32_t finished_works = session->finished_works;*/

    /*uint32_t n;*/
    /*for ( n = 0 ; n < finished_works ; n++ ){*/
        /*msg_response_t *response = alloc_response(0, RESULT_SUCCESS);*/
        /*notice_log("session_send_data() %d/%d", n, finished_works);*/
        /*session_send_data(session, (char *)response, sizeof(*response), after_response_to_client);*/
        /*zfree(response);*/
    /*}*/

    /*__sync_sub_and_fetch(&session->finished_works, finished_works);*/

}

static UNUSED void session_timer_cb(uv_timer_t *timer_handle, int status) 
{
    session_t *session = (session_t*)timer_handle->data;
    if ( session->waiting_for_close == 1 ) {
        if ( session_is_waiting(session)) {
            info_log("Start to close session in session_timer_cb()");
            uv_timer_stop(&session->timer_handle);

            uv_shutdown_t* shutdown_req = (uv_shutdown_t*)zmalloc(sizeof(uv_shutdown_t));
            shutdown_req->data = session;
            uv_shutdown(shutdown_req, &session->connection.handle.stream, after_shutdown);
        }
    }
}

static UNUSED void session_async_cb(uv_async_t *async_handle, int status) 
{
    session_t *session = (session_t*)async_handle->data;

    /* -------- Shutdown -------- */
    if ( session->waiting_for_close == 1 ) {
        info_log("Want to close session in session_async_cb()");

        uv_timer_stop(&session->timer_handle);

        uv_shutdown_t* shutdown_req = (uv_shutdown_t*)zmalloc(sizeof(uv_shutdown_t));
        shutdown_req->data = session;
        uv_shutdown(shutdown_req, &session->connection.handle.stream, after_shutdown);
    }
}

/* ==================== after_read() ==================== */ 
/*
 * nread <= DEFAULT_CONN_BUF_SIZE 64 * 1024
 */

UNUSED static void after_read(uv_stream_t *handle, ssize_t nread, const uv_buf_t *buf) 
{
    session_FROM_UV_HANDLE(handle);

    conn_buf_t *cob = container_of(buf->base, conn_buf_t, base);
    assert(session == cob->session);

    if ( nread > 0 ) {
        /* -------------------------------------------------------------------- */
        /* Normal handle. */

        cob->write_head += nread;
        session->connection.total_bytes += nread;
        __sync_add_and_fetch(&cob->remain_bytes, nread);

        trace_log("\n........\nfd(%d) block(%d) nread=%zu bytes. write_head:%d, remain_bytes=%d, total_bytes=%d\n", session_fd(session), cob->blockid, nread, cob->write_head, cob->remain_bytes, session->connection.total_bytes);

        enqueue_recv_queue(session, cob);
        pthread_yield();

        /* FIXME */
        while ( server->cached_bytes > MAX_CACHED_BYTES ) {
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
            warning_log("read error. errno=-%d total_bytes=%d", ~(uint32_t)(nread - 1), session->connection.total_bytes);
        }

        /* -------- Shutdown -------- */
        session->waiting_for_close = 1;
        info_log("waiting_for_colose=1");
        /*uv_shutdown_t* shutdown_req = (uv_shutdown_t*)zmalloc(sizeof(uv_shutdown_t));*/
        /*shutdown_req->data = session;*/
        /*uv_shutdown(shutdown_req, handle, after_shutdown);*/

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
    session_FROM_UV_HANDLE(handle);

    /* -------- cob -------- */
    uint32_t buf_len = sizeof(conn_buf_t);
    conn_buf_t *cob = (conn_buf_t *)zmalloc(buf_len);
    cob->session = session;
    init_cob(cob);
    __sync_add_and_fetch(&session->cached_bytes, buf_len);
    __sync_add_and_fetch(&server->cached_bytes, buf_len);

    /* XXX Calculate total blocks for debug. */
    cob->blockid = __sync_add_and_fetch(&session->total_blocks, 1);

    /* -------- set uv_buf -------- */
    buf->base = cob->base;
    buf->len = cob->len;

}

/* ==================== create_session() ==================== */ 
session_t* create_session(server_t *server)
{
    static uint32_t seq_no = 0;

    /* -------- session -------- */
    session_t *session = (session_t*)zmalloc(sizeof(session_t));
    memset(session, 0, sizeof(session_t));
    session->server = server;
	session->sid.seq_no = seq_no++;
	pthread_mutex_init(&session->recv_pending_lock, NULL);
	pthread_cond_init(&session->recv_pending_cond, NULL);
	pthread_mutex_init(&session->send_pending_lock, NULL);
	pthread_cond_init(&session->send_pending_cond, NULL);
	pthread_mutex_init(&session->after_response_lock, NULL);
	pthread_cond_init(&session->after_response_cond, NULL);

    empty_session(session);

    if ( create_session_coroutine(session) != 0 ) {
        destroy_session(session);
        return NULL;
    }

    int ret;

    /* -------- loop -------- */
    uv_loop_t *loop =  server->tcp_handle.loop;

    uv_async_init(loop, &session->async_handle, session_async_cb);
    session->async_handle.data = session;

    uv_idle_t *idle_handle = &session->idle_handle;
    idle_handle->data = session;
    uv_idle_init(loop, idle_handle);
    uv_idle_start(idle_handle, session_idle_cb);

    uv_timer_init(loop, &session->timer_handle);
    session->timer_handle.data = session;
    uv_timer_start(&session->timer_handle, session_timer_cb, 1000, 10000);

    /* -------- tcp_handle -------- */
    uv_tcp_t *tcp_handle = &session->connection.handle.tcp;
    tcp_handle->data = session;

    /* -------- uv_tcp_init -------- */
    ret = uv_tcp_init(loop, tcp_handle);
    if ( ret != 0 ) { 
        destroy_session(session);
        error_log("uv_tcp_init() failed. ret = %d", ret); 
        return NULL; 
    }

    /* -------- uv_accept -------- */
    ret = uv_accept((uv_stream_t*)&server->tcp_handle, (uv_stream_t*)tcp_handle);
    if ( ret != 0 ) { 
        destroy_session(session);
        error_log("uv_accept() failed. ret = %d", ret); 
        return NULL; 
    }


    /* -------- session_rx_on -------- */
    ret = session_rx_on(session);
    if ( ret != 0 ) { 
        destroy_session(session);
        error_log("session_rx_on() failed. ret = %d", ret); 
        return NULL; 
    }

    return session;
}

/* ************************************************************
 *
 *                 Session Private Functions
 *
 * ************************************************************/

/* ==================== destroy_session() ==================== */ 
void destroy_session(session_t *session)
{
    assert(session != NULL);

    destroy_session_coroutine(session);

    listRelease(session->responseQueue);

    pthread_mutex_destroy(&session->recv_pending_lock);
    pthread_cond_destroy(&session->recv_pending_cond);
    pthread_mutex_destroy(&session->send_pending_lock);
    pthread_cond_destroy(&session->send_pending_cond);
    pthread_mutex_destroy(&session->after_response_lock);
    pthread_cond_destroy(&session->after_response_cond);

    zfree(session);
}

/* ==================== close_session() ==================== */ 
void close_session(session_t *session)
{
    session_rx_off(session);
    destroy_session(session);
}

/* ==================== session_incref() ==================== */ 
UNUSED void session_incref(session_t *session)
{
	if ( session != NULL ){
		__sync_add_and_fetch(&session->refcnt, 1);
    }
}

/* ==================== session_decref() ==================== */ 
UNUSED void session_decref(session_t *session)
{
	if ( session != NULL ) {
		int refcnt = __sync_sub_and_fetch(&session->refcnt, 1);
        if ( refcnt == 0 ) {
            close_session(session);
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
    session_t *session = cob->session;
    if ( session != NULL ){
        __sync_sub_and_fetch(&session->cached_bytes, sizeof(conn_buf_t));
        server_t *server = session->server;
        if ( server != NULL ){
            __sync_sub_and_fetch(&server->cached_bytes, sizeof(conn_buf_t));
        }
    }

    zfree(cob);
}

/* ==================== empty_session() ==================== */ 
void empty_session(session_t *session)
{
    session->session_status = SESSION_STATUS_HEAD;
    session->f = NULL;
    session->refcnt = 1;
    session->stop = 0;
    session->waiting_for_close = 0;
    session->total_blocks = 0;
    session->total_received_buffers = 0;
    session->total_saved_buffers = 0;
	session->sid.nodeid = 0;
    INIT_LIST_HEAD(&session->rx_block_queue);

    session->connection.session = session;
    session->connection.total_bytes = 0;
    session->cached_bytes = 0;
    session->finished_works = 0;

    session->responseQueue = listCreate();
    listSetFreeMethod(session->responseQueue, byte_block_free);

    session->handle_request = session_handle_request;

    /* Init conn_buf_t field in session->connection.*/
    session->connection.cob.session = session;
    init_cob(&session->connection.cob);
}

extern void *session_rx_coroutine(void *opaque);
extern void *session_tx_coroutine(void *opaque);
/* ==================== create_session_coroutine() ==================== */ 
int create_session_coroutine(session_t *session)
{
    /*info_log("Create session coroutine.");*/
    session->rx_coroutine = coroutine_create(session_rx_coroutine);
    if ( session->rx_coroutine == NULL ) {
        error_log("Cann't create coroutine session->rx_coroutine");
        return -1;
    }

    session->tx_coroutine = coroutine_create(session_tx_coroutine);
    if ( session->tx_coroutine == NULL ) {
        error_log("Cann't create coroutine session->tx_coroutine");
        return -1;
    }

    return 0;
}

/* ==================== destroy_session_coroutine() ==================== */ 
int destroy_session_coroutine(session_t *session)
{
    /*info_log("Destroy session coroutine.");*/
    if ( session->rx_coroutine != NULL ){
        coroutine_delete(session->rx_coroutine);
        session->rx_coroutine = NULL;
    }

    if ( session->tx_coroutine != NULL ){
        coroutine_delete(session->tx_coroutine);
        session->tx_coroutine = NULL;
    }

    return 0;
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

void session_finish_saving_buffer(session_t *session){
    pthread_mutex_lock(&session->recv_pending_lock);{
        session->total_saved_buffers++;
    } pthread_mutex_unlock(&session->recv_pending_lock);
}

/* ==================== session_is_waiting() ==================== */ 
int session_is_waiting(session_t *session)
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

/* ==================== session_tx_on() ==================== */ 
int session_tx_on(session_t *session)
{
    return 0;
}

/* ==================== session_tx_off() ==================== */ 
void session_tx_off(session_t *session)
{
}

/* ==================== remove_from_recv_queue() ==================== */ 
void remove_from_recv_queue(session_t *session, conn_buf_t *cob) 
{
    /*session_t *session = cob->session;*/

    work_queue_t *wq = get_recv_queue_by_session(session->server, session);

    if ( wq != NULL ) {
        remove_work(wq, &cob->rx_block_list);
    }
}

/* ==================== enqueue_recv_queue() ==================== */ 
void enqueue_recv_queue(session_t *session, conn_buf_t *cob)
{
    pthread_mutex_lock(&session->recv_pending_lock);{
        session->total_received_buffers++;
    } pthread_mutex_unlock(&session->recv_pending_lock);

    work_queue_t *wq = get_recv_queue_by_session(session->server, session);

    if ( wq != NULL ) {
        enqueue_work(wq, (void*)cob);
    }
}

/* ==================== dequeue_recv_queue() ==================== */ 
conn_buf_t *dequeue_recv_queue(session_t *session)
{
    work_queue_t *wq = get_recv_queue_by_session(session->server, session);

    if ( wq != NULL ) {
        return dequeue_work(wq);
    }

    return NULL;
}

/* ==================== check_data_crc32() ==================== */ 
int check_data_crc32(int requestid, msg_arg_t *argCRC32, msg_arg_t *argData)
{
    uint32_t crc = *((uint32_t*)argCRC32->data);
    uint32_t crc1 = crc32(0, argData->data, argData->size);

    if ( crc != crc1 ) {
        error_log("requestid(%d) upload crc32: %d, Data crc32: %d", requestid, crc, crc1);
        return -1;
    }

    return 0;
}

