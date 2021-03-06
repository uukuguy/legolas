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
#include "service.h"
#include "session.h"
#include "work.h"
#include "logger.h"
#include "crc32.h"
#include "uv.h"
/*#include "react_utils.h"*/

/*#include "coroutine.h"*/

static pthread_once_t _tls_init_once = PTHREAD_ONCE_INIT;
static int _sockbuf_init_complete = 0;
static pthread_key_t _current_sockbuf;

sockmsg_t *sockmsg_new(void)
{
    sockmsg_t *sockmsg = (sockmsg_t*)zmalloc(sizeof(sockmsg_t));
    memset(sockmsg, 0, sizeof(sockmsg_t));
    sockmsg->state = SOCKMSG_WAITING_HEAD;
    sockmsg->except_bytes = sizeof(message_t);
    return sockmsg;
}

void sockmsg_free(sockmsg_t *sockmsg)
{
    assert(sockmsg != NULL);
    zfree(sockmsg);
}

static void _sockbuf_tls_init(void)
{
    if ( pthread_key_create(&_current_sockbuf, NULL) != 0){
        return;
    }
    _sockbuf_init_complete = 1;
}

sockbuf_t *get_current_sockbuf(void)
{
    pthread_once(&_tls_init_once, _sockbuf_tls_init);
    if (!_sockbuf_init_complete)
        return NULL;
    return pthread_getspecific(_current_sockbuf);
}   

void set_current_sockbuf(sockbuf_t *sockbuf)
{
    pthread_once(&_tls_init_once, _sockbuf_tls_init);
    if (!_sockbuf_init_complete)
        return;
    pthread_setspecific(_current_sockbuf, sockbuf);
}

/* -------- libcoro -------- */
#ifdef USE_LIBCORO
void coroutine_enter(session_t *session, void *opaque)
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

    
void coroutine_enter(session_t *session, void *opaque)
{
    greenlet_switch_to(session->rx_coroutine, opaque);
}

void coroutine_yield(session_t *session)
{
    greenlet_t *current = greenlet_current();
    greenlet_t *parent = greenlet_parent(current);
    greenlet_switch_to(parent, NULL);
}

#define YIELD_AND_CONTINUE \
    coroutine_yield(session); \
    sockbuf = session->sockbuf; \
    continue;

#endif

/* -------- coroutine -------- */
#ifdef USE_COROUTINE

void coroutine_enter(session_t *session, void *opaque)
{
    _coroutine_enter(session->rx_coctx, opaque);
}

void coroutine_yield(session_t *session)
{
    _coroutine_yield();
}

#define YIELD_AND_CONTINUE \
    coroutine_yield(session); \
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
    assert(sockbuf != NULL);
    assert(buf != NULL);

    session_t *session = sockbuf->session;
    assert(session != NULL);

    /**
     * Keep read data from sockbuf, until buf is fullfill count bytes.
     */
	uint32_t done = 0;
    while ( count > 0 ) {
        /* FIXME coroutine */
        /*session = coroutine_self_data();*/
        /*sockbuf = session->sockbuf;*/
        sockbuf = get_current_sockbuf();

        assert(sockbuf != NULL);
        assert(sockbuf->write_head >= sockbuf->read_head);

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

        /*TRACE_LOG_SESSION_SOCKBUF("Called do_read_data().");*/
        /*YIELD_AND_CONTINUE;*/
        coroutine_yield(session); 
        continue;
    }

}

void dump_sockbuf(sockbuf_t *sockbuf, const char *dumpfile_name)
{
    int file = open(dumpfile_name, O_CREAT | O_TRUNC | O_WRONLY | O_SYNC, 0640);
    write(file, sockbuf, sizeof(sockbuf_t));
    close(file);

}

/* ==================== session_do_read() ==================== */ 
int session_do_read(sockbuf_t *sockbuf, message_t **p_message)
{
    UNUSED session_t *session = sockbuf->session;

    /* -------- do_read_data:message_header -------- */
    /*TRACE_LOG_SESSION_SOCKBUF("Call do_read_data() for message_header.");*/

    message_t message_header;

    do_read_data(sockbuf, &message_header, sizeof(message_header));
    /* FIXME coroutine */
    /*session = coroutine_self_data();*/
    /*sockbuf = session->sockbuf;*/
    sockbuf = get_current_sockbuf();

    /* FIXME coroutine */
    /*sockbuf = coroutine_self_data();*/
    /*session = sockbuf->session;*/

    /* -------- check message message_header -------- */
    if ( check_message((message_t*)&message_header) != 0) {
        WARNING_LOG_SESSION_SOCKBUF("Check magic_code in message message_header failed!");
        /*dump_sockbuf(sockbuf, "error_sockbuf.dat");*/
        /*dump_sockbuf(&session->last_sockbuf, "last_sockbuf.dat");*/
        /*abort();*/
        return -1;
    } else {
        /*TRACE_LOG_SESSION_sockbuf("Check magic_code in message message_header OK!");*/
        /*trace_log("message_header.data_length=%d", message_header.data_length);*/
    }

    /* FIXME 2014-10-21 16:43:50 */
    /*memcpy(&session->last_sockbuf, sockbuf, sizeof(sockbuf_t));*/

    /* -------- do_read_data:data -------- */
    /*TRACE_LOG_SESSION_SOCKBUF("Call do_read_data() for data.");*/

    message_t *message = (message_t*)zmalloc(sizeof(message_header) + message_header.data_length);
    memset(message, 0, sizeof(message_header) + message_header.data_length);
    memcpy(message, &message_header, sizeof(message_header));

    do_read_data(sockbuf, message->data, message->data_length);

    if ( message->data_length > 0 ){
        uint32_t crc = crc32(0, (const char *)message->data, message->data_length);
        if ( crc != message->crc32_data ){
            warning_log("Check session(%d) message(%d) data crc32 failed.", session->id, message->id);
        }
    }

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
#ifdef USE_COROUTINE
void *session_rx_coroutine(void *opaque)
#endif
{
    /*REACT_ACTION_START(session_rx_coroutine);*/

    UNUSED int ret;
    session_t *session = (session_t*)opaque;

    while ( !session->stop ){

        /** ----------------------------------------
         *    Keep read data
         *  ---------------------------------------- */

        /* FIXME coroutine */
        /*session = coroutine_self_data();*/
        /*sockbuf_t *sockbuf = session->sockbuf;*/
        sockbuf_t *sockbuf = get_current_sockbuf();

        message_t *message = NULL;
        ret = session_do_read(sockbuf, &message);

        if ( ret == 0 ) {
            assert(message != NULL);

            if ( session->service->callbacks.handle_message != NULL ){
                ret = session->service->callbacks.handle_message(session, message);
            }
            zfree(message);
            message = NULL;

            /*sockbuf = session->sockbuf;*/
            sockbuf = get_current_sockbuf();

            assert(sockbuf->write_head >= sockbuf->read_head);
            if ( sockbuf->write_head == sockbuf->read_head ){
                coroutine_yield(session); 
            }

        } else {
            coroutine_yield(session); 
            continue;
            /*YIELD_AND_CONTINUE;*/
        }
    }


    /*REACT_ACTION_STOP(session_rx_coroutine);*/

#ifdef USE_CGREENLET
    return NULL;
#endif
#ifdef USE_COROUTINE
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
    session->rx_coroutine = greenlet_new(session_rx_coroutine, greenlet_current(), 64 * 1024);
    if ( session->rx_coroutine == NULL ) {
        error_log("Cann't create coroutine session->rx_coroutine");
        return -1;
    }
#endif

    /* -------- libcoro -------- */
#ifdef USE_LIBCORO
    /*coro_stack_alloc(&session->main_stack, 64 * 1024);*/
    /*coro_create(&session->main_coctx, NULL, NULL, session->main_stack.sptr, session->main_stack.ssze);*/
    coro_create(&session->main_coctx, NULL, NULL, session->main_stack, 64 * 1024);
    /*coro_create(&session->main_coctx, NULL, NULL, NULL, 0);*/

    /*coro_stack_alloc(&session->rx_stack, 64 * 1024);*/
    /*coro_create(&session->rx_coctx, session_rx_coroutine, session, session->rx_stack.sptr, session->rx_stack.ssze);*/
    coro_create(&session->rx_coctx, session_rx_coroutine, session, session->rx_stack, 64 * 1024);
#endif

#ifdef USE_COROUTINE
    session->rx_coctx = coroutine_create(session_rx_coroutine);
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
    /*if ( session->rx_stack.sptr != NULL )*/
        /*coro_stack_free(&session->rx_stack);*/
    /*if ( session->main_stack.sptr != NULL )*/
        /*coro_stack_free(&session->main_stack);*/
    /*coro_destroy(&session->rx_coctx);*/
    /*coro_destroy(&session->main_coctx);*/
#endif

#ifdef USE_COROUTINE
    if ( session->rx_coctx != NULL ){
        coroutine_delete(session->rx_coctx);
        session->rx_coctx = NULL;
    }
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
    /*REACT_ACTION_START(session_consume_sockbuf);*/

    session_t *session = sockbuf->session;

    /*pthread_mutex_lock(&session->recv_pending_lock);*/

    if ( likely( sockbuf->remain_bytes > 0 ) ) {
        /* FIXME coroutine */
        /*session->sockbuf = sockbuf;*/
        set_current_sockbuf(sockbuf);
        /*sockbuf_t *s1 = get_current_sockbuf();*/

        coroutine_enter(session, session);

    } else {
        /* -------- remain bytes == 0 -------- */
    }

    sockbuf_free(sockbuf);

    __sync_add_and_fetch(&session->total_saved_buffers, 1);

    /*pthread_yield();*/
    /*sched_yield();*/

    /*pthread_mutex_unlock(&session->recv_pending_lock);*/

    /*REACT_ACTION_STOP(session_consume_sockbuf);*/
}

/* ==================== session_after_read() ==================== */ 
/*
 * nread <= DEFAULT_CONN_BUF_SIZE 64 * 1024
 */

int consume_a_message(message_context_t *msgctx, char *data, uint32_t data_size)
{
    int current_state = msgctx->current_state;

    if ( current_state == ConsumeState_WAITING_HEAD ){
        if ( data_size >= msgctx->except_size ){
            char *msgbuf = (char*)&msgctx->msgheader;
            memcpy(&msgbuf[sizeof(message_t) - msgctx->except_size], data, msgctx->except_size);
            msgctx->consumed_size += msgctx->except_size;


            message_t *message = &msgctx->msgheader;
            if ( message->magic_code[0] == 'l' &&
                    message->magic_code[1] == 'e' &&
                    message->magic_code[2] == 'g' &&
                    message->magic_code[3] == 'o' &&
                    message->magic_code[4] == 'l' &&
                    message->magic_code[5] == 'a' &&
                    message->magic_code[6] == 's' ) { 

                if ( msgctx->msgheader.data_length == 0 ){ 
                    /*assert(msgctx->total_message_new == msgctx->total_message_free);*/
                    msgctx->message = (message_t*)zmalloc(sizeof(message_t));
                    msgctx->total_message_new++;
                    memset(msgctx->message, 0, sizeof(message_t));
                    memcpy(msgctx->message, &msgctx->msgheader, sizeof(message_t));

                    msgctx->except_size = sizeof(message_t);
                    msgctx->current_state = ConsumeState_WAITING_HEAD;
                    return 0;
                } else {
                    /*assert(msgctx->total_message_new == msgctx->total_message_free);*/
                    msgctx->message = (message_t*)zmalloc(sizeof(message_t) + msgctx->msgheader.data_length);
                    msgctx->total_message_new++;
                    memset(msgctx->message, 0, sizeof(message_t) + msgctx->msgheader.data_length);
                    memcpy(msgctx->message, &msgctx->msgheader, sizeof(message_t));
                    msgctx->writed_body_size = 0;

                    msgctx->except_size = msgctx->msgheader.data_length;
                    msgctx->current_state = ConsumeState_WAITING_BODY;
                    return -1;
                }
            } else {
                warning_log("Check magic_code in message(%d) failed!", message->id);
                msgctx->except_size = sizeof(message_t);
                msgctx->current_state = ConsumeState_WAITING_HEAD;
                return -1;
            }
        } else {
            char *msgbuf = (char*)&msgctx->msgheader;
            memcpy(&msgbuf[sizeof(message_t) - msgctx->except_size], data, data_size);
            msgctx->consumed_size += data_size;
            
            msgctx->except_size -= data_size;
            /*msgctx->current_state = ConsumeState_WAITING_HEAD;*/
            return -1;
        }
    } else if ( current_state == ConsumeState_WAITING_BODY ){
        if ( data_size >= msgctx->except_size ){
            msgctx->consumed_size += msgctx->except_size;

            char *msgdata = (char*)msgctx->message->data;
            memcpy(&msgdata[msgctx->writed_body_size], data, msgctx->except_size);
            msgctx->writed_body_size += msgctx->except_size;

            msgctx->except_size = sizeof(message_t);
            msgctx->current_state = ConsumeState_WAITING_HEAD;
            return 0;
        } else {
            char *msgdata = (char*)msgctx->message->data;
            memcpy(&msgdata[msgctx->writed_body_size], data, data_size);
            msgctx->writed_body_size += data_size;

            msgctx->consumed_size += data_size;
            msgctx->except_size -= data_size;
            /*msgctx->current_state = ConsumeState_WAITING_BODY;*/
            return -1;
        }
    }
    error_log("Unknown state.!");
    abort();
    return -1;
}

void message_context_clear_message(message_context_t *msgctx)
{
    if ( msgctx->message != NULL ){
        zfree(msgctx->message);
        msgctx->message = NULL;

        msgctx->total_message_free++;
    }
}

void dump_message_data(message_t *message, const char *dumpfile_name)
{
    int file = open(dumpfile_name, O_CREAT | O_TRUNC | O_WRONLY | O_SYNC, 0640);
    write(file, message->data, message->data_length);
    close(file);

}

void test_consume_sockbuf(session_t *session, sockbuf_t *sockbuf)
{
    message_context_t *msgctx = &session->msgctx;
    char *data = sockbuf->base;
    uint32_t data_size = sockbuf->write_head;
   
    while ( 1 ){
        if ( consume_a_message(msgctx, data, data_size) == 0 ){
            // recevie a message OK.
            message_t *message = msgctx->message;

            /*if ( check_message(message) != 0) {*/
                /*warning_log("Check magic_code in message(%d) failed!", message->id);*/
            uint32_t crc = crc32(0, (const char *)message->data, message->data_length);
            if ( crc != message->crc32_data ){
                warning_log("Check data crc32 in session(%d) message(%d) data_length(%d) failed!", session->id, message->id, message->data_length);
                /*dump_message_data(message, "message_data.dat");*/
                
            }

                /*msgctx->consumed_size = 0;*/
                /*sockbuf_free(sockbuf);*/
                /*message_context_clear_message(msgctx);*/
                /*[>__sync_add_and_fetch(&session->finished_works, 1);<]*/
                /*session_response(session, RESULT_ERR_UNKNOWN);*/
                /*break;*/
            /*}*/

            if ( session->service->callbacks.handle_message != NULL ){
                session->service->callbacks.handle_message(session, message);
            }

            message_context_clear_message(msgctx);
        }
        assert(msgctx->consumed_size <= sockbuf->write_head);
        if ( msgctx->consumed_size == sockbuf->write_head ){
            msgctx->consumed_size = 0;
            sockbuf_free(sockbuf);
            break;
        }
        data = &sockbuf->base[msgctx->consumed_size];
        data_size = sockbuf->write_head - msgctx->consumed_size;
    }
}

void session_after_read(uv_stream_t *handle, ssize_t nread, const uv_buf_t *buf) 
{
    session_t *session = (session_t*)handle->data;


    if ( nread > 0 ) {

        /* -------- sockmsg -------- */
        /*sockmsg_t *sockmsg = session->sockmsg;*/

        /*if ( nread != sockmsg->except_bytes ){*/
            /*warning_log("nread:%zu != sockmsg->except_bytes:%d", nread, sockmsg->except_bytes);*/
        /*} else {*/
            /*info_log("nread:%zu == sockmsg->except_bytes:%d", nread, sockmsg->except_bytes);*/
        /*}*/

        /*if ( sockmsg->state == SOCKMSG_WAITING_HEAD ){*/
            /*if ( nread != sizeof(message_t) ){*/
                /*warning_log("SKIP sockmsg! sockmsg received %zu bytes. It's not except %zu bytes.", nread, sizeof(message_t));*/
            /*} else {*/
                /*sockmsg->state = SOCKMSG_WAITING_BODY;*/
            /*}*/
        /*} else if ( sockmsg->state == SOCKMSG_WAITING_BODY ){*/
            /*if ( nread != sockmsg->message->data_length ){*/
                /*warning_log("SKIP sockmsg! sockmsg received %zu bytes. It's not except %d bytes.", nread, sockmsg->message->data_length);*/
            /*} else {*/
                /*if ( session->service->callbacks.handle_message != NULL ){*/
                    /*session->service->callbacks.handle_message(session, session->sockmsg->message);*/
                /*}*/
                /*zfree(session->sockmsg->message);*/
                /*session->sockmsg->message = NULL;*/
            /*}*/
            /*sockmsg->state = SOCKMSG_WAITING_HEAD;*/
        /*} else {*/
            /*error_log("Unexcept else in session_after_read()!");*/
            /*abort();*/
        /*}*/

        /* -------- sockbuf -------- */
        sockbuf_t *sockbuf = container_of(buf->base, sockbuf_t, base);
        assert(session == sockbuf->session);

        sockbuf->write_head = nread;
        sockbuf->remain_bytes = nread;

        __sync_add_and_fetch(&session->total_received_buffers, 1);
        __sync_add_and_fetch(&session->connection.total_bytes, nread);


        /*notice_log("--- blocks: %d nread: %d total_reaceived: %zu", session->total_received_buffers, (int32_t)nread, session->connection.total_bytes);*/

        /* -------------------------------------------------------------------- */
        /* Normal handle. */


        /* FIXME 2014-10-10 23:20:15 */

        /*enqueue_parse_queue(session, sockbuf);*/

        /*test_consume_sockbuf(session, sockbuf);*/

        if ( session->service->callbacks.consume_sockbuf != NULL ){
            session->service->callbacks.consume_sockbuf(sockbuf);
        } else {
            session_consume_sockbuf(sockbuf);
        }


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
            info_log("It's UV__EOF (%d) %s. session(%d) finished works(%d)", (int32_t)nread, uv_strerror((int32_t)nread), session->id, session->finished_works);
        } else if ( nread == UV__ECONNRESET ){
            info_log("It's UV__ECONNRESET (%d) %s. session(%d) finished works(%d)", (int32_t)nread, uv_strerror((int32_t)nread), session->id, session->finished_works);
        } else {
            /*warning_log("read error. errno=-%d(%zu) total_bytes=%zu", ~(uint32_t)(nread - 1), nread, session->connection.total_bytes);*/
            warning_log("read error. errno=-%d err:%s. session(%d) finished works(%d)", (int32_t)nread, uv_strerror((int32_t)nread), session->id, session->finished_works);
        }

        /* -------- Shutdown -------- */
        session_rx_off(session);
        session->waiting_for_close = 1;

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


UNUSED void session_alloc_sockmsg(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) 
{
    session_t *session = (session_t*)handle->data;

    sockmsg_t *sockmsg = session->sockmsg;

    if ( sockmsg->state == SOCKMSG_WAITING_HEAD ){
        sockmsg->except_bytes = sizeof(message_t);

        buf->base = (char*)&sockmsg->message_header;
        buf->len = sockmsg->except_bytes;
    } else if ( sockmsg->state == SOCKMSG_WAITING_BODY ){
        sockmsg->except_bytes = sockmsg->message_header.data_length;

        uint32_t msg_size = sizeof(sockmsg->message_header) + sockmsg->message_header.data_length;
        message_t *message = (message_t*)zmalloc(msg_size);
        memset(message, 0, sizeof(msg_size));
        memcpy(message, &sockmsg->message_header, sizeof(sockmsg->message_header));
        sockmsg->message = message;

        buf->base = (char *)sockmsg->message->data;
        buf->len = sockmsg->except_bytes;
    } else {
        error_log("Unexcept else in session_alloc()!");
        abort();
    }

    /*notice_log("session_alloc() except_bytes:%d", sockmsg->except_bytes);*/
}

