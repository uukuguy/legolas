/**
 * @file   sockbuf_message.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-07-07 02:49:56
 * 
 * @brief  
 *      将底层网络通讯数据转换成message_t。
 *      1. 设置session->callbacks.handle_message。
 *          typedef typedef int (*handle_message_cb)(session_t*, message_t *);
 *      2. 调用session_waiting_message(session_t*)
 * 
 * 
 */

#include "common.h"
/*#include "server.h"*/
#include "session.h"
#include "work.h"
#include "logger.h"

/*#include "coroutine.h"*/

/* -------- libcoro -------- */
#ifdef USE_LIBCORO
void coroutine_enter(session_t *session)
{
    coro_transfer(&session->main_coctx, &session->rx_coctx);
}

void coroutine_yield(session_t *session)
{
    coro_transfer(&session->rx_coctx, &session->main_coctx);
}

#define YIELD_AND_CONTINUE \
    coroutine_yield(session); \
    sockbuf = session->sockbuf; \
    continue;

#endif


/* -------- cgreenlet -------- */
#ifdef USE_CGREENLET
/*void coroutine_set_data(coroutine_t *coroutine, void *data)*/
/*{*/
    /*coroutine->gr_arg = data;*/
/*}*/

/*void* coroutine_self_data(void)*/
/*{*/
    /*greenlet_t *current = greenlet_current();*/
    /*return current->gr_arg;*/
/*}*/

    
void coroutine_enter(coroutine_t *coroutine, void *opaque)
{
    greenlet_switch_to(coroutine, opaque);
}

void coroutine_yield(void)
{
    greenlet_t *current = greenlet_current();
    greenlet_t *parent = greenlet_parent(current);
    greenlet_switch_to(parent, NULL);
}

#define YIELD_AND_CONTINUE \
    coroutine_yield(); \
    sockbuf = session->sockbuf; \
    continue;

#endif


/* ==================== do_read_data() ==================== */ 
/*
 * Keep read data into buf from sockbuf. If there is no more data in sockbuf,
 * return control to parent thread by call coroutine_yield().
 */
void do_read_data(sockbuf_t *sockbuf, void *buf, size_t count)
{
    session_t *session = sockbuf->session;

    assert(sockbuf != NULL);
    assert(buf != NULL);

    /**
     * Keep read data from sockbuf, until buf is fullfill count bytes.
     */
	uint32_t done = 0;
    while ( count > 0 ) {
        /* FIXME coroutine */
        /*session = coroutine_self_data();*/
        sockbuf = session->sockbuf;

        uint32_t len = sockbuf->write_head - sockbuf->read_head;
        /*trace_log("Next to ...: blockid(%d), len(%d), count(%zu)", sockbuf->blockid, len, count);*/

        /**
         * There are enough bytes(len) can be copied(count) into target buf.
         */
        if ( len >= count ) {
            /* -------- Copy count bytes -------- */ 
            memcpy(buf + done, sockbuf->base + sockbuf->read_head, count); 
            done += count; 
            sockbuf->read_head += count; 
            __sync_sub_and_fetch(&sockbuf->remain_bytes, count);

            count = 0; 

            break;
        }

        /**
         * Need count bytes, but there are just len (len < count) bytes.
         * Just copy all bytes(len) in sockbuf into target buf.
         */
        if ( len > 0 ) {
            /* -------- Copy len bytes -------- */ 
            memcpy(buf + done, sockbuf->base + sockbuf->read_head, len); 
            done += len; 
            sockbuf->read_head += len; 
            __sync_sub_and_fetch(&sockbuf->remain_bytes, len);

            count -= len; 
        }

        TRACE_LOG_SESSION_SOCKBUF("Called do_read_data().");
        YIELD_AND_CONTINUE;
    }
}

/* ==================== session_do_read() ==================== */ 
int session_do_read(sockbuf_t *sockbuf, message_t **p_message)
{
    UNUSED session_t *session = sockbuf->session;

    /* -------- do_read_data:message_header -------- */
    TRACE_LOG_SESSION_SOCKBUF("Call do_read_data() for message_header.");

    message_t message_header;

    do_read_data(sockbuf, &message_header, sizeof(message_header));
    /* FIXME coroutine */
    /*session = coroutine_self_data();*/
    sockbuf = session->sockbuf;

    /* FIXME coroutine */
    /*sockbuf = coroutine_self_data();*/
    /*session = sockbuf->session;*/

    /* -------- check message message_header -------- */
    if ( check_message((message_t*)&message_header) != 0) {
        WARNING_LOG_SESSION_SOCKBUF("Check magic_code in message message_header failed!");
        return -1;
    } else {
        /*TRACE_LOG_SESSION_sockbuf("Check magic_code in message message_header OK!");*/
        /*trace_log("message_header.data_length=%d", message_header.data_length);*/
    }

    /* -------- do_read_data:data -------- */
    TRACE_LOG_SESSION_SOCKBUF("Call do_read_data() for data.");

    message_t *message = (message_t*)zmalloc(sizeof(message_header) + message_header.data_length);
    memcpy(message, &message_header, sizeof(message_header));

    do_read_data(sockbuf, message->data, message->data_length);

    /* FIXME coroutine */
    /*sockbuf = coroutine_self_data();*/
    /*session = sockbuf->session;*/

    *p_message = message;

    /*pthread_yield();*/
    /*sched_yield();*/

    return 0;
}

/* ==================== session_rx_handler() ==================== */ 
/**
 * Hnadling received buffer coroutine.
 * Called by recv_request().
 *
 * COMMON_HEADER_FIELDS:
 *      uint8_t         magic_code[8]; 
 *      uint32_t        id; 
 *      uint8_t         msg_type; 
 *      uint8_t         msg_version; 
 *      uint16_t        reserved; 
 *      uint32_t        data_length 
 *
 *      uint8_t         data[];
 *          When id == 0 
 *              uint8_t key[<=128];
 *              uint32_t object_size;
 *
 *          COMMON_TAIL_FIELD:
 *              uint8_t md5[];
 *              uint8_t data[];
 */

#ifdef USE_CGREENLET
void *session_rx_coroutine(void *opaque)
#endif
#ifdef USE_LIBCORO
void session_rx_coroutine(void *opaque)
#endif
{
    UNUSED int ret;
    session_t *session = (session_t*)opaque;

    while ( !session->stop ){

        /** ----------------------------------------
         *    Keep read data
         *  ---------------------------------------- */

        /* FIXME coroutine */
        /*session = coroutine_self_data();*/
        sockbuf_t *sockbuf = session->sockbuf;
        message_t *message = NULL;
        ret = session_do_read(sockbuf, &message);

        /* FIXME coroutine */
        /*session = coroutine_self_data();*/
        sockbuf = session->sockbuf;

        /* FIXME coroutine */
        /*sockbuf = coroutine_self_data();*/
        /*session = sockbuf->session;*/

        if ( ret == 0 ) {
            assert(message != NULL);

            if ( session->callbacks.handle_message != NULL ){
                ret = session->callbacks.handle_message(session, message);
            }
            zfree(message);
            message = NULL;
        } else {
            YIELD_AND_CONTINUE;
        }
    }

#ifdef USE_CGREENLET
    return NULL;
#endif
}


/* ==================== create_session_coroutine() ==================== */ 
int create_session_coroutine(session_t *session)
{
    /*info_log("Create session coroutine.");*/
    /*session->rx_coroutine = coroutine_create(session_rx_coroutine);*/

    /* -------- cgreenlet -------- */
#ifdef USE_CGREENLET
    session->rx_coroutine = greenlet_new(session_rx_coroutine, greenlet_current(), 0);
    if ( session->rx_coroutine == NULL ) {
        error_log("Cann't create coroutine session->rx_coroutine");
        return -1;
    }
#endif

    /* -------- libcoro -------- */
#ifdef USE_LIBCORO
    /*coro_stack_alloc(&session->main_stack, 1024);*/
    /*coro_create(&session->main_coctx, NULL, NULL, session->main_stack.sptr, session->main_stack.ssze);*/
    coro_create(&session->main_coctx, NULL, NULL, session->main_stack, 64 * 1024);
    /*coro_create(&session->main_coctx, NULL, NULL, NULL, 0);*/

    /*coro_stack_alloc(&session->rx_stack, 1024);*/
    /*coro_create(&session->rx_coctx, session_rx_coroutine, session, session->rx_stack.sptr, session->rx_stack.ssze);*/
    coro_create(&session->rx_coctx, session_rx_coroutine, session, session->rx_stack, 64 * 1024);
#endif

    /*session->tx_coroutine = coroutine_create(session_tx_coroutine);*/
    /*if ( session->tx_coroutine == NULL ) {*/
        /*error_log("Cann't create coroutine session->tx_coroutine");*/
        /*return -1;*/
    /*}*/

    return 0;
}

/* ==================== destroy_session_coroutine() ==================== */ 
int destroy_session_coroutine(session_t *session)
{
    /*info_log("Destroy session coroutine.");*/

    /*coroutine_delete(session->rx_coroutine);*/

    /* -------- cgreenlet -------- */
#ifdef USE_CGREENLET
    if ( session->rx_coroutine != NULL ){
        greenlet_destroy(session->rx_coroutine);
        session->rx_coroutine = NULL;
    }
#endif

    /* -------- libcoro -------- */
#ifdef USE_LIBCORO
    /*coro_stack_free(&session->rx_stack);*/
    /*coro_stack_free(&session->main_stack);*/
    /*coro_destroy(&session->rx_coctx);*/
    /*coro_destroy(&session->main_coctx);*/
#endif

    /*if ( session->tx_coroutine != NULL ){*/
        /*coroutine_delete(session->tx_coroutine);*/
        /*session->tx_coroutine = NULL;*/
    /*}*/

    return 0;
}

/* ==================== session_consume_sockbuf() ==================== */ 
void session_consume_sockbuf(sockbuf_t *sockbuf)
{
    session_t *session = sockbuf->session;

    if ( likely( sockbuf->remain_bytes > 0 ) ) {
        /* FIXME coroutine */
        session->sockbuf = sockbuf;

        /* -------- libcoro -------- */
#ifdef USE_LIBCORO
        coroutine_enter(session);
#endif

        /* -------- cgreenlent -------- */
#ifdef USE_CGREENLET
        coroutine_enter(session->rx_coroutine, session);
#endif

    } else {
        /* -------- remain bytes == 0 -------- */
    }

    sockbuf_free(sockbuf);

    __sync_add_and_fetch(&session->total_saved_buffers, 1);

    /*pthread_yield();*/
    sched_yield();

}



/* ==================== after_read() ==================== */ 
/*
 * nread <= DEFAULT_CONN_BUF_SIZE 64 * 1024
 */

void after_read(uv_stream_t *handle, ssize_t nread, const uv_buf_t *buf) 
{
    session_t *session = (session_t*)handle->data;


    if ( nread > 0 ) {
        sockbuf_t *sockbuf = container_of(buf->base, sockbuf_t, base);
        assert(session == sockbuf->session);

        /* -------------------------------------------------------------------- */
        /* Normal handle. */

        sockbuf->write_head += nread;
        session->connection.total_bytes += nread;
        __sync_add_and_fetch(&sockbuf->remain_bytes, nread);

        trace_log("\n........\nfd(%d) block(%d) nread=%zu bytes. write_head:%d, remain_bytes=%d, total_bytes=%d\n", session_fd(session), sockbuf->blockid, nread, sockbuf->write_head, sockbuf->remain_bytes, session->connection.total_bytes);


        __sync_add_and_fetch(&session->total_received_buffers, 1);
        if ( session->callbacks.consume_sockbuf != NULL ){
            session->callbacks.consume_sockbuf(sockbuf);
        } else {
            session_consume_sockbuf(sockbuf);
        }

        /*session_response(session, RESULT_SUCCESS);*/
        /*sockbuf_free(sockbuf);*/
        /*session_finish_saving_buffer(session);*/

        /* FIXME */
        /*while ( server->cached_bytes > MAX_CACHED_BYTES ) {*/
            /*usleep(10 * 1000);*/
            /*pthread_yield();*/
            /*sched_yield();*/
        /*}*/


    } else if (nread < 0) {
        /* -------------------------------------------------------------------- */
        /* Error or EOF. Must shutdown. */

        /* -------- UV__ENOBUFS -------- */
        if ( nread == UV__ENOBUFS ) {
            trace_log("It's UV__ENOBUFS");
            return;
        }

        /* -------- UV__EOF -------- */
        if ( nread == UV__EOF ) {
            /*info_log("It's UV__EOF");*/
        } else {
            warning_log("read error. errno=-%d(%zu) total_bytes=%d", ~(uint32_t)(nread - 1), nread, session->connection.total_bytes);
        }

        /* -------- Shutdown -------- */
        session_rx_off(session);
        session->waiting_for_close = 1;
        info_log("waiting_for_colose=1");
        /*uv_shutdown_t* shutdown_req = (uv_shutdown_t*)zmalloc(sizeof(uv_shutdown_t));*/
        /*shutdown_req->data = session;*/
        /*uv_shutdown(shutdown_req, handle, after_shutdown);*/

        return;
    } else { 
        /* -------------------------------------------------------------------- */
        /* nread == 0 Everything OK, but nothing read. */

        return;
    }

}

/* ==================== session_alloc() ==================== */ 

void session_alloc(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) 
{
    session_t *session = (session_t*)handle->data;

    /* -------- sockbuf -------- */
    sockbuf_t *sockbuf = sockbuf_new(session);

    uint32_t buf_len = sizeof(sockbuf_t);
    __sync_add_and_fetch(&session->cached_bytes, buf_len);
    /*__sync_add_and_fetch(&server->cached_bytes, buf_len);*/

    /* XXX Calculate total blocks for debug. */
    sockbuf->blockid = __sync_add_and_fetch(&session->total_blocks, 1);

    /* -------- set uv_buf -------- */
    buf->base = sockbuf->base;
    buf->len = sockbuf->len;

}

/* ==================== session_waiting_message() ==================== */ 
int session_waiting_message(session_t *session)
{
    return uv_read_start((uv_stream_t*)&session_stream(session), session_alloc, after_read);
}


