/**
    listIter *iter = listGetIterator(queue, AL_START_HEAD);
    listNode *node = NULL;
    while ( (node = listNext(iter)) != NULL ) { 
        cob = (struct conn_buf_t *)listNodeValue(iter);
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
#include "md5.h"
#include "uv.h"

#include "coroutine.h"
#include <pthread.h>
#include <uuid/uuid.h>
#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#include <assert.h>

static uint32_t total_fopen = 0;
static uint32_t total_fclose = 0;

extern UNUSED void response_to_client(struct session_info_t *session_info, enum MSG_RESULT result); /* in session_send.c */

#define YIELD_AND_CONTINUE \
    trace_log("Ready to yield and continue."); \
    coroutine_yield(); \
    cob = coroutine_self_data(); \
    trace_log("After YIELD coroutine_self_data(). cob=%p", cob); \
    session_info = cob->session_info; \
    continue; 

/* ==================== do_read_data() ==================== */ 
/*
 * Keep read data into buf from cob. If there is no more data in cob,
 * return control to parent thread by call coroutine_yield().
 */
void do_read_data(struct conn_buf_t *cob, void *buf, size_t count)
{
    UNUSED session_info_t *session_info = cob->session_info;

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

/* ==================== check_data_md5() ==================== */ 
int check_data_md5(int requestid, struct msg_arg_t *argMd5, struct msg_arg_t *argData)
{
    struct md5_value_t *md5Keep = (struct md5_value_t*)argMd5->data;
    struct md5_value_t md5Data;
    md5(&md5Data, (uint8_t *)argData->data, argData->size);

    /*notice_log("id(%d) block(%d) Data md5: h0:%X h1:%X h2:%X h3:%X", requestid, md5Data.h0, md5Data.h1, md5Data.h2, md5Data.h3);*/
    if ( check_md5(md5Keep, &md5Data) != 0 ) {
        error_log("requestid(%d) Data md5: h0:%X h1:%X h2:%X h3:%X", requestid, md5Data.h0, md5Data.h1, md5Data.h2, md5Data.h3);
        return -1;
    }

    return 0;
}

/* ==================== session_do_read() ==================== */ 
int session_do_read(conn_buf_t *cob, msg_request_t **p_request)
{
    session_info_t *session_info = cob->session_info;

    /* -------- do_read_data:request_header -------- */
    TRACE_LOG_SESSION_COB("Call do_read_data() for request_header.");

    struct msg_request_t request_header;

    do_read_data(cob, &request_header, sizeof(request_header));

    cob = coroutine_self_data();
    session_info = cob->session_info;

    /* -------- check message request_header -------- */
    if ( !check_msg((msg_header_t*)&request_header) ) {
        WARNING_LOG_SESSION_COB("Check magic_code in message request_header failed!");
        /*TRACE_DATA_TO_FILE("result/request_header.dat", &request_header, sizeof(request_header));*/

        /*session_info->stop = 1;   */
        return -1;
    } else {
        /*TRACE_LOG_SESSION_COB("Check magic_code in message request_header OK!");*/
        /*trace_log("request_header.data_length=%d", request_header.data_length);*/
    }

    /* -------- do_read_data:data -------- */
    TRACE_LOG_SESSION_COB("Call do_read_data() for data.");

    struct msg_request_t *request = (struct msg_request_t*)zmalloc(sizeof(request_header) + request_header.data_length);
    memcpy(request, &request_header, sizeof(request_header));

    do_read_data(cob, request->data, request->data_length);

    cob = coroutine_self_data();
    session_info = cob->session_info;

    *p_request = request;

    return 0;
}

typedef struct block_info_t {
    uint32_t id;
    uint32_t file_size;
    char key[NAME_MAX];
    char *data;
    uint32_t data_size;
} block_info_t;

/* ==================== parse_block() ==================== */ 
int parse_block(struct session_info_t *session_info, struct msg_request_t *request, struct block_info_t *block_info)
{
    /* -------- message args -------- */
    struct msg_arg_t *arg = (struct msg_arg_t*)request->data;
    struct msg_arg_t *argMd5 = NULL;
    block_info->id = request->id;
    if ( request->id == 0 ) {
        /* -------- argKey -------- */
        struct msg_arg_t *argKey = arg;
        if ( argKey->size > 0 ) {
            uint32_t keylen = argKey->size < NAME_MAX - 1 ? argKey->size : NAME_MAX - 1;
            memcpy(block_info->key, argKey->data, keylen);
            block_info->key[keylen] = '\0';
        } else {
            uuid_t uuid;
            uuid_generate(uuid);
            uuid_unparse(uuid, block_info->key);
        }

        /* -------- argFileSize -------- */
        struct msg_arg_t *argFileSize = next_arg(argKey);
        block_info->file_size = *((uint32_t*)argFileSize->data);

        /*trace_log("\n~~~~~~~~ fd(%d) block(%d) ~~~~~~~~\n Try to open new file. \n key=%s file_size=%d\n", session_fd(session_info), cob->blockid, argKey->data, file_size);*/

        /* -------- argMd5 -------- */
        argMd5 = next_arg(argFileSize);
    } else {
        argMd5 = arg;
    }

    struct msg_arg_t *argData = next_arg(argMd5);

    /*debug_log("fd(%d) block(%d) argData: size=%d", session_fd(session_info), cob->blockid, argData->size);*/
    if ( argData->size > 0 ) {

        /**
         * Check data md5 value.
         */

        if ( check_data_md5(request->id, argMd5, argData) != 0 ){
            error_log("fd(%d) request_id(%d) Check buffer md5 failed.", session_fd(session_info), request->id);
            return -1;
        }
    } 

    block_info->data = argData->data;
    block_info->data_size = argData->size;

    return 0;
}

/* ==================== session_write_block() ==================== */ 
int session_write_block(struct session_info_t *session_info, struct block_info_t *block_info)
{
    UNUSED int r = 0;
    char *aligned_buf = NULL;
    uint32_t write_bytes = block_info->data_size;

    if ( block_info->data_size > 0 ) {
        if ( session_info->f != NULL ){
            uint32_t nowrited_bytes = block_info->file_size - session_info->total_writed; 
            assert(block_info->data_size <= nowrited_bytes);

            char *write_buf = block_info->data;

            /* is lastest buffer. */
            if ( write_bytes == nowrited_bytes ){
                write_bytes = (block_info->data_size + 511) & ~(511);
                assert(write_bytes >= block_info->data_size);
            } else {
                assert(block_info->data_size % 512 == 0);
                write_bytes = block_info->data_size;
            }
            assert(write_bytes % 512 == 0);

            r = posix_memalign((void**)&aligned_buf, 512, DEFAULT_CONN_BUF_SIZE);
            if ( r != 0 ){
                error_log("posix_memalign() return %d. msg: %s", r, strerror(r));
                abort();
            }

            memset(aligned_buf, 0, write_bytes);

            assert(write_bytes >= block_info->data_size);
            memcpy(aligned_buf, block_info->data, block_info->data_size);
            write_buf = aligned_buf;

            if ( storage_write_file(&session_info->server_info->storage_info, write_buf, write_bytes, session_info->f) < write_bytes ) {
                zfree(aligned_buf);
                return -1;
            }
            zfree(aligned_buf);

            session_info->total_writed += write_bytes;
            pthread_yield();

        } else {
            error_log("fd(%d) session_info->f == NULL", session_fd(session_info));
            return -1;
        }
    } 

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
 *              uint32_t file_size;
 *
 *          COMMON_TAIL_FIELD:
 *              uint8_t md5[];
 *              uint8_t data[];
 */

#define FREE_REQUEST_YIELD_AND_CONTINUE \
    zfree(request); \
    request = NULL; \
    cob = coroutine_self_data(); \
    session_info = cob->session_info; \
    YIELD_AND_CONTINUE;

void *session_rx_handler(void *opaque)
{
    UNUSED int ret;
    block_info_t block_info;

    msg_request_t *request = NULL;
    struct conn_buf_t *cob = (struct conn_buf_t*)opaque;
    struct session_info_t *session_info = cob->session_info;

    while ( 1 ){
        /** ----------------------------------------
         *    Keep read data
         *  ---------------------------------------- */

        ret = session_do_read(cob, &request);
        cob = coroutine_self_data();
        session_info = cob->session_info;
        if ( ret != 0 || request == NULL ) {
            YIELD_AND_CONTINUE;
        }

        /** ----------------------------------------
         *    Parse block
         *  ---------------------------------------- */

        if ( parse_block(session_info, request, &block_info) != 0 ){
            error_log("parse_block() failed. key:%s", block_info.key);
            FREE_REQUEST_YIELD_AND_CONTINUE;
        }
        /*notice_log("block_info.file_size:%d", block_info.file_size);*/

        /** ----------------------------------------
         *    Open target file for write if possible.
         *  ---------------------------------------- */

        if ( block_info.id == 0 ) {

            session_info->total_writed = 0;

            session_info->f = storage_open_file(&session_info->server_info->storage_info, block_info.key, "wb+");
            if ( session_info->f == NULL ){
                error_log("storage_open_file() failed. key:%s", block_info.key);
                FREE_REQUEST_YIELD_AND_CONTINUE;
            }

            total_fopen++;
            trace_log("fd(%d) block(%d) storage_file(%p) fopen %s.", session_fd(session_info), cob->blockid, session_info->f, block_info.key);
        }

        /** ----------------------------------------
         *    Write block!
         *  ---------------------------------------- */

        /*trace_log("fd(%d) block(%d) argData: size=%d", session_fd(session_info), cob->blockid, argData->size);*/
        if ( session_write_block(session_info, &block_info) != 0 ) {
            error_log("session_write_block() failed.");
            FREE_REQUEST_YIELD_AND_CONTINUE;
        }


        /** ----------------------------------------
         *    Close storage.
         *  ---------------------------------------- */

        if ( session_info->total_writed >= block_info.file_size ){
            /* -------- fclose -------- */
            trace_log("fd(%d) block(%d) storage_file(%p) fclose %s. total_writed:%d file_size:%d", session_fd(session_info), cob->blockid, session_info->f, block_info.key, session_info->total_writed, block_info.file_size);
            total_fclose++;
            storage_close_file(&session_info->server_info->storage_info, session_info->f);
            session_info->f = NULL;
            session_info->total_writed = 0;
            pthread_yield();

            /* -------- response -------- */

            /*session_rx_off(session_info);*/
            response_to_client(session_info, RESULT_SUCCESS);
            /*session_rx_on(session_info);*/

        }

        zfree(request);
        request = NULL;

    }

    return NULL;
}

/* ==================== recv_cob_in_queue() ==================== */ 
void recv_cob_in_queue(struct conn_buf_t *cob)
{
    session_info_t *session_info = cob->session_info;

    if ( likely( cob->remain_bytes > 0 ) ) {

        /**
         * coroutine enter session_rx_handler() 
         */
        coroutine_enter(session_info->rx_co, cob);

        trace_log("return from co_call.");

        /**
         * too many requests or not ?
         */

        if ( too_many_requests(session_info) ) {
            session_info->stop = 1;
        } else {
            session_rx_on(session_info);
        }

    } else {
        /* -------- remain bytes == 0 -------- */
    }

    delete_cob(cob);

    /*if ( session_info->f == NULL )*/
    /*response_to_client(session_info, RESULT_SUCCESS);*/

    session_finish_saving_buffer(session_info);

    if ( session_is_waiting(session_info) ){
        if ( session_info->waiting_for_close == 1 ) { 
            pthread_cond_signal(&session_info->recv_pending_cond);
        }
    }
    pthread_yield();

}

/* ==================== recv_request() ==================== */ 
/**
 * Running in a thread in recv_queue.
 * One thread per session and many sessions per thread.
 */
void recv_request(struct work_queue *wq)
{
    void *nodeData = NULL;
    while ( (nodeData = dequeue_work(wq)) != NULL ){
       recv_cob_in_queue((struct conn_buf_t*)nodeData);
    }
}

