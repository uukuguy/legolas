/**
 * @file   session_recv.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-05-05 13:23:11
 * 
 * @brief  
 * 
 * 
 */

#include "server.h"
#include "session.h"
#include "protocol.h"
/*#include "md5.h"*/
#include "uv.h"
#include "adlist.h"
#include "skiplist.h"
#include "vnode.h"
#include "object.h"
#include "kvdb.h"
#include "coroutine.h"
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#include <assert.h>


#define YIELD_AND_CONTINUE \
    trace_log("Ready to yield and continue."); \
    coroutine_yield(); \
    cob = coroutine_self_data(); \
    trace_log("After YIELD coroutine_self_data(). cob=%p", cob); \
    session = cob->session; \
    continue; 

/* ==================== do_read_data() ==================== */ 
/*
 * Keep read data into buf from cob. If there is no more data in cob,
 * return control to parent thread by call coroutine_yield().
 */
void do_read_data(conn_buf_t *cob, void *buf, size_t count)
{
    UNUSED session_t *session = cob->session;

    assert(cob != NULL);
    assert(buf != NULL);
    assert(count > 0);

    /**
     * Keep read data from cob, until buf is fullfill count bytes.
     */
	uint32_t done = 0;
    while ( count > 0 ) {
        uint32_t len = cob->write_head - cob->read_head;
        /*trace_log("Next to ...: blockid(%d), len(%d), count(%zu)", cob->blockid, len, count);*/

        /**
         * There are enough bytes(len) can be copied(count) into target buf.
         */
        if ( len >= count ) {
            /* -------- Copy count bytes -------- */ 
            memcpy(buf + done, cob->base + cob->read_head, count); 
            done += count; 
            cob->read_head += count; 
            __sync_sub_and_fetch(&cob->remain_bytes, count);

            count = 0; 

            break;
        }

        /**
         * Need count bytes, but there are just len (len < count) bytes.
         * Just copy all bytes(len) in cob into target buf.
         */
        if ( len > 0 ) {
            /* -------- Copy len bytes -------- */ 
            memcpy(buf + done, cob->base + cob->read_head, len); 
            done += len; 
            cob->read_head += len; 
            __sync_sub_and_fetch(&cob->remain_bytes, len);

            count -= len; 
        }

        /**
         * coroutine swap to parent. (coroutine_enter in recv_request())
         */ 
        YIELD_AND_CONTINUE;
    }
}

/* ==================== session_do_read() ==================== */ 
int session_do_read(conn_buf_t *cob, msg_request_t **p_request)
{
    session_t *session = cob->session;

    /* -------- do_read_data:request_header -------- */
    TRACE_LOG_SESSION_COB("Call do_read_data() for request_header.");

    msg_request_t request_header;

    do_read_data(cob, &request_header, sizeof(request_header));

    cob = coroutine_self_data();
    session = cob->session;

    /* -------- check message request_header -------- */
    if ( !check_msg((msg_header_t*)&request_header) ) {
        WARNING_LOG_SESSION_COB("Check magic_code in message request_header failed!");
        /*TRACE_DATA_TO_FILE("result/request_header.dat", &request_header, sizeof(request_header));*/

        /*session->stop = 1;   */
        return -1;
    } else {
        /*TRACE_LOG_SESSION_COB("Check magic_code in message request_header OK!");*/
        /*trace_log("request_header.data_length=%d", request_header.data_length);*/
    }

    /* -------- do_read_data:data -------- */
    TRACE_LOG_SESSION_COB("Call do_read_data() for data.");

    msg_request_t *request = (msg_request_t*)zmalloc(sizeof(request_header) + request_header.data_length);
    memcpy(request, &request_header, sizeof(request_header));

    do_read_data(cob, request->data, request->data_length);

    cob = coroutine_self_data();
    session = cob->session;

    *p_request = request;

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

void *session_rx_coroutine(void *opaque)
{
    UNUSED int ret;

    msg_request_t *request = NULL;
    conn_buf_t *cob = (conn_buf_t*)opaque;
    session_t *session = cob->session;

    while ( !session->stop ){

        /** ----------------------------------------
         *    Keep read data
         *  ---------------------------------------- */

        ret = session_do_read(cob, &request);
        cob = coroutine_self_data();
        session = cob->session;
        if ( ret != 0 || request == NULL ) {
            YIELD_AND_CONTINUE;
        }

        ret = 0;
        if ( session->handle_request != NULL ){
            ret = session->handle_request(session, request);
        /*session_response_to_client(session, RESULT_SUCCESS);*/
        }

        zfree(request);
        request = NULL;

        if ( ret != 0 ) {
            YIELD_AND_CONTINUE;
        }
    }

    return NULL;
}

/* ==================== recv_queue_process_cob() ==================== */ 
void recv_queue_process_cob(conn_buf_t *cob)
{
    session_t *session = cob->session;

    if ( likely( cob->remain_bytes > 0 ) ) {

        /**
         * coroutine enter session_rx_handler() 
         */
        coroutine_enter(session->rx_coroutine, cob);

        trace_log("return from co_call.");

        /**
         * too many requests or not ?
         */

    } else {
        /* -------- remain bytes == 0 -------- */
    }

    delete_cob(cob);

    session_finish_saving_buffer(session);

    /*if ( session_is_waiting(session) ){*/
        /*if ( session->waiting_for_close == 1 ) { */
            /*pthread_cond_signal(&session->recv_pending_cond);*/
        /*}*/
    /*}*/
    pthread_yield();

}

/* ==================== recv_queue_handle_request() ==================== */ 
/**
 * Running in a thread in recv_queue.
 * One thread per session and many sessions per thread.
 */
void recv_queue_handle_request(work_queue_t *wq)
{
    void *nodeData = NULL;
    while ( (nodeData = dequeue_work(wq)) != NULL ){
       recv_queue_process_cob((conn_buf_t*)nodeData);
    }
}

