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
#include "protocol.h"
#include "uv.h"
#include "md5.h"
#include "lockfree_queue.h"
#include <uuid/uuid.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <linux/fs.h>

#include "coroutine.h"
/*#include "co_routine.h"*/

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
        /*session_rx_off(session_info);*/
        trace_log("fd(%d) Ready to call uv_write. finished_works:%d", session_fd(session_info), finished_works);

        /*uv_read_stop((uv_stream_t*)&session_stream(session_info));*/
        /*pthread_mutex_lock(&session_info->after_response_lock);*/
        /*session_info->is_sending = 1;*/

        int r = uv_write(write_rsp,
                &session_stream(session_info),
                /*(uv_stream_t*)idle_handle,*/
                &rbuf,
                1,
                NULL);
            /*after_response_to_client);*/
        /*pthread_mutex_unlock(&session_info->after_response_lock);*/

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
    
    /*session_decref(session_info);*/
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

int process_cob_head(conn_buf_t *cob)
{
    session_info_t *session_info = cob->session_info;
    msg_request_t *request_header = (msg_request_t*)cob->base;
    /*TRACE_DATA_TO_FILE("data/request_header.dat", request_header, sizeof(msg_header_t));*/
    if ( !check_msg((msg_header_t*)request_header) ) {
        WARNING_LOG_SESSION_COB("Check magic_code in message request_header failed!");
        /*TRACE_DATA_TO_FILE("data/request_header.dat", &request_header, sizeof(request_header));*/
        return -1;
    }

    cob->remain_bytes -= sizeof(msg_request_t);
    cob->read_tail = sizeof(msg_request_t);
    cob->least_size = request_header->data_length;
    trace_log("request_header: id=%d, data_length=%d", request_header->id, request_header->data_length);
    session_info->session_status = SESSION_STATUS_BODY;

    return 0;
}

int process_cob_body(conn_buf_t *cob)
{
    trace_log("enter process_cob_body().");
    session_info_t *session_info = cob->session_info;
    msg_request_t *request = (msg_request_t*)cob->base;


    /*TRACE_DATA_TO_FILE("data/request.dat", request, sizeof(msg_header_t) + request->data_length);*/
    /*exit(-1);*/

    struct msg_arg_t *arg = (struct msg_arg_t*)request->data;
    /*TRACE_DATA_TO_FILE("data/request_data.dat", request->data, request->data_length);*/
    /*exit(-1);*/
    struct msg_arg_t *argMd5 = NULL;
    if ( request->id == 0 ) {

        session_info->total_writed = 0;

        /* -------- argKey -------- */
        struct msg_arg_t *argKey = arg;

        /* -------- argFileSize -------- */
        struct msg_arg_t *argFileSize = next_arg(argKey);
        session_info->file_size = *((uint32_t*)argFileSize->data);

        trace_log("\n~~~~~~~~ fd(%d) block(%d) ~~~~~~~~\n Try to open new file. \n key=%s file_size=%d\n", session_fd(session_info), cob->blockid, argKey->data, session_info->file_size);

        /* -------- argMd5 -------- */
        argMd5 = next_arg(argFileSize);

        /**
         * Open target file for write.
         */
        {
            /* -------- key or uuid for file name -------- */
            /*char *key = &session_info->key[0];*/
            char key[NAME_MAX];
            if ( argKey->size > 0 ) {
                trace_log("has key: keysize=%d", argKey->size);
                uint32_t keyLen = argKey->size < NAME_MAX - 1 ? argKey->size : NAME_MAX - 1;
                memcpy(key, argKey->data, keyLen);
                key[keyLen] = '\0';
                trace_log("keylen:%d key:%s", keyLen, key);
            } else {
                trace_log("key uuid");
                uuid_t uuid;
                uuid_generate(uuid);
                uuid_unparse(uuid, key);
            }
            trace_log("key = %s", key);
            memcpy(session_info->key, key, strlen(key));

            /* -------- openfile -------- */
            session_info->f = storage_open_file(&session_info->server_info->storage_info, key, "wb+");
            if ( session_info->f == NULL ){
                error_log("storage_open_file() failed. key:%s", key);
                return -1;
            }
        }

    } else {
        /* -------- argMd5 -------- */
        argMd5 = arg;
    }

    struct msg_arg_t *argData = next_arg(argMd5);

    if ( argData->size > 0 ) {

        /**
         * Check data md5 value.
         */

        /*if ( check_data_md5(request->id, cob->blockid, argMd5, argData) != 0 ){*/
        /*error_log("fd(%d) block(%d) Check buffer md5 failed.", session_fd(session_info), cob->blockid);*/
        /*zfree(request);*/
        /*request = NULL;*/
        /*YIELD_AND_CONTINUE;*/
        /*}*/

        /**
         * Do write!
         */

        if ( session_info->f != NULL ){
            if ( storage_write_file(&session_info->server_info->storage_info, argData->data, argData->size, session_info->f) < argData->size ) {
                error_log("storage_write_file() failed.");
                return -1;
            }
            session_info->total_writed += argData->size;
        } else {
            error_log("fd(%d) block(%d) session_info->f == NULL", session_fd(session_info), cob->blockid);
        }
    } 

    if ( session_info->total_writed >= session_info->file_size ){
        /* -------- fclose -------- */
        trace_log("fd(%d) block(%d) fclose %s.", session_fd(session_info), cob->blockid, session_info->key);
        /*fclose(session_info->f);*/
        storage_close_file(&session_info->server_info->storage_info, session_info->f);
        session_info->f = NULL;
        session_info->total_writed = 0;

        /* -------- response -------- */

        /*session_rx_off(session_info);*/
        /*response_to_client(session_info, RESULT_SUCCESS);*/
        /*session_rx_on(session_info);*/

    }

    assert(cob->remain_bytes >= request->data_length);
    cob->remain_bytes -= request->data_length;
    cob->read_tail += request->data_length;
    if ( cob->remain_bytes > 0 ){
        memcpy(cob->base, cob->base + cob->read_tail, cob->remain_bytes);
    }
    cob->write_head = cob->remain_bytes;
    cob->read_tail = 0;
    cob->least_size = sizeof(msg_request_t);
    session_info->session_status = SESSION_STATUS_HEAD;
    return 0;
}

int process_cob_end(conn_buf_t *cob)
{
    session_info_t *session_info = cob->session_info;

    session_info->session_status = SESSION_STATUS_HEAD;
    return 0;
}

int process_cob(conn_buf_t *cob)
{
    session_info_t *session_info = cob->session_info;
    UNUSED server_info_t *server_info = session_info->server_info;

    if ( session_info->session_status == SESSION_STATUS_HEAD ){
        process_cob_head(cob);
    } else if ( session_info->session_status == SESSION_STATUS_BODY ){
        process_cob_body(cob);
    } else if ( session_info->session_status == SESSION_STATUS_END ){
    }

    return -1;
}

/* ==================== after_read() ==================== */ 
/*
 * nread <= DEFAULT_CONN_BUF_SIZE 64 * 1024
 */
UNUSED static void __after_read(uv_stream_t *handle, ssize_t nread, const uv_buf_t *buf) 
{
    SESSION_INFO_FROM_UV_HANDLE(handle, session_info, server_info);

    conn_buf_t *cob = &session_info->connection.cob;
    assert(session_info == cob->session_info);

    if ( nread > 0 ) {
        /* -------------------------------------------------------------------- */
        /* Normal handle. */

        cob->write_head += nread;
        session_info->connection.total_bytes += nread;
        __sync_add_and_fetch(&cob->remain_bytes, nread);

        trace_log("\n........\nfd(%d) block(%d) nread=%zu bytes. write_head:%d, remain_bytes=%d, least_size=%d, read_tail=%d\n", session_fd(session_info), cob->blockid, nread, cob->write_head, cob->remain_bytes, cob->least_size, cob->read_tail);

        while ( cob->write_head >= cob->least_size + cob->read_tail ){
            process_cob(cob);
        }

        /* FIXME */
        while ( server_info->cached_bytes > MAX_CACHED_BYTES ) {
            usleep(10);
        }


    } else if (nread < 0) {
        /* -------------------------------------------------------------------- */
        /* Error or EOF. Must shutdown. */

        /*destroy_cob(cob);*/

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

        destroy_cob(cob);
        return;
    }

}

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

        queue_into_recv_queue(session_info, cob);
        pthread_yield();

        /* FIXME */
        while ( server_info->cached_bytes > MAX_CACHED_BYTES ) {
            usleep(10 * 1000);
            pthread_yield();
        }


    } else if (nread < 0) {
        /* -------------------------------------------------------------------- */
        /* Error or EOF. Must shutdown. */

        destroy_cob(cob);

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

        destroy_cob(cob);
        return;
    }

}

/* ==================== session_alloc() ==================== */ 
UNUSED static void __session_alloc(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) 
{
    SESSION_INFO_FROM_UV_HANDLE(handle, session_info, server_info);

    conn_buf_t *cob = &session_info->connection.cob;
    buf->base = cob->base + cob->write_head;
    buf->len = cob->len - cob->write_head;
}

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
        /*pthread_mutex_lock(&session_info->recv_pending_lock);*/
		__sync_add_and_fetch(&session_info->refcnt, 1);
        /*pthread_mutex_unlock(&session_info->recv_pending_lock);*/
    }
}

/* ==================== session_decref() ==================== */ 
UNUSED void session_decref(struct session_info_t *session_info)
{
	if ( session_info != NULL ) {
        /*pthread_mutex_lock(&session_info->recv_pending_lock);*/
		int refcnt = __sync_sub_and_fetch(&session_info->refcnt, 1);
        /*pthread_mutex_unlock(&session_info->recv_pending_lock);*/
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

/* ==================== destroy_cob() ==================== */ 
void destroy_cob(conn_buf_t *cob)
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
    /*session_info->rx_co = co_create((void(*)(void *))session_rx_handler, NULL, NULL, 4096);*/
    /*co_create(&session_info->rx_co, NULL, session_rx_handler, session_info);*/
    if ( session_info->rx_co == NULL ) {
        error_log("Cann't create coroutine session_info->rx_co");
        return -1;
    }

    session_info->tx_co = coroutine_create(session_tx_handler);
    /*session_info->tx_co = co_create((void(*)(void *))session_tx_handler, NULL, NULL, 4096);*/
    /*co_create(&session_info->tx_co, NULL, session_tx_handler, session_info);*/
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
        /*co_delete(session_info->rx_co);*/
        /*co_release(session_info->rx_co);*/
        session_info->rx_co = NULL;
    }

    if ( session_info->tx_co != NULL ){
        coroutine_delete(session_info->tx_co);
        /*co_delete(session_info->tx_co);*/
        /*co_release(session_info->tx_co);*/
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

/*conn_buf_t *dequeue_recv_queue(session_info_t *session_info)*/
/*{*/
    /*list_dequeue(*/
/*}*/

/* ==================== remove_from_recv_queue() ==================== */ 
void remove_from_recv_queue(session_info_t *session_info, conn_buf_t *cob) 
{
    /*session_info_t *session_info = cob->session_info;*/

    int fd = session_fd(session_info);

    server_info_t *server_info = session_info->server_info;

    struct work_queue *wq = server_info->recv_queue[fd % RECV_THREADS];
    /*lockfree_queue_t *wq = server_info->recv_queue[fd % RECV_THREADS];*/

    if ( wq != NULL ) {
        remove_work(wq, &cob->rx_block_list);
    }
}

/* ==================== queue_into_recv_queue() ==================== */ 
void queue_into_recv_queue(session_info_t *session_info, conn_buf_t *cob)
{
    trace_log("enqueue recv queue. cob=%p", cob);
    /*session_info_t *session_info = cob->session_info;*/

    /*session_rx_off(session_info);*/
    /*session_incref(session_info);*/
    pthread_mutex_lock(&session_info->recv_pending_lock);{
        session_info->total_received_buffers++;
    } pthread_mutex_unlock(&session_info->recv_pending_lock);

    int fd = session_fd(session_info);    

    struct work_queue *wq = session_info->server_info->recv_queue[fd % RECV_THREADS];
    /*lockfree_queue_t *wq = server_info->recv_queue[fd % RECV_THREADS];*/

    if ( wq != NULL ) {
        /*enqueue_work(wq, &cob->rx_block_list);*/
        enqueue_work(wq, (void*)cob);
    }
}

