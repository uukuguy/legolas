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

#include "session.h"
#include "protocol.h"
#include "md5.h"
#include "uv.h"

#include "coroutine.h"
/*#include "pcl.h"*/
/*#include "co_routine.h"*/

#include "lockfree_queue.h"
#include <pthread.h>
#include <uuid/uuid.h>
#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#include <assert.h>

static uint32_t total_fopen = 0;
static uint32_t total_fclose = 0;

/*extern pthread_key_t key_actived_cob;*/
extern UNUSED void response_to_client(struct session_info_t *session_info, enum MSG_RESULT result); /* in session_send.c */

#define YIELD_AND_CONTINUE \
    trace_log("Ready to yield and continue."); \
    coroutine_yield(); \
    cob = coroutine_self_data(); \
    trace_log("After YIELD coroutine_self_data(). cob=%p", cob); \
    session_info = cob->session_info; \
    continue; 

/*#define YIELD_AND_CONTINUE \*/
    /*trace_log("Ready to yield and continue."); \*/
    /*co_resume(); \*/
    /*cob = co_get_data(co_current()); \*/
    /*trace_log("After YIELD co_get_data(). cob=%p", cob); \*/
    /*session_info = cob->session_info; \*/
    /*continue; */

/*#define YIELD_AND_CONTINUE \*/
    /*trace_log("Ready to yield and continue."); \*/
    /*co_yield_ct(); \*/
    /*cob = pthread_getspecific(key_actived_cob); \*/
    /*trace_log("After YIELD . cob=%p", cob); \*/
    /*session_info = cob->session_info; \*/
    /*continue; */

/* ==================== do_read_data() ==================== */ 
/*
 * Keep read data into buf from cob. If there is no more data in cob,
 * return control to parent thread by call coroutine_yield().
 */
void do_read_data(struct conn_buf_t *cob, void *buf, size_t count)
{
    /*struct session_info_t *session_info = cob->session_info;*/
    /*TRACE_LOG_SESSION_COB("Before do read data.");*/
    /*trace_log("Try to read %zu bytes data.", count);*/

    UNUSED session_info_t *session_info = NULL;

    assert(cob != NULL);
    assert(buf != NULL);
    assert(count > 0);

    /**
     * Keep read data from cob, until buf is fullfill count bytes.
     */
	uint32_t done = 0;
    while ( count > 0 ) {
        /*cob = coroutine_self_data();*/
        /*cob = co_get_data(co_current());*/
        /*session_info = cob->session_info;*/

        uint32_t len = cob->write_head - cob->read_head;
        /*TRACE_LOG_SESSION_COB("Next to while ( count > 0 )");*/
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

            /*trace_log("1. Copy out %zu bytes. count remain 0 bytes.", count);*/
            /*TRACE_LOG_SESSION_COB("1. Copy out");*/

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

            /*trace_log("2. Copy out %d bytes. count remain %zu bytes.", len, count - len);*/
            /*TRACE_LOG_SESSION_COB("2. Copy out");*/

            count -= len; 
        }

        /**
         * coroutine swap to parent. (coroutine_enter in recv_request())
         */ 
        /*coroutine_yield();*/
        YIELD_AND_CONTINUE;
    }

    /*TRACE_LOG_SESSION_COB("After do read data.");*/
}

int check_data_md5(int requestid, int blockid, struct msg_arg_t *argMd5, struct msg_arg_t *argData)
{
    struct md5_value_t *md5Keep = (struct md5_value_t*)argMd5->data;
    struct md5_value_t md5Data;
    md5(&md5Data, (uint8_t *)argData->data, argData->size);

    /*notice_log("id(%d) block(%d) Data md5: h0:%X h1:%X h2:%X h3:%X", requestid, blockid, md5Data.h0, md5Data.h1, md5Data.h2, md5Data.h3);*/
    if ( check_md5(md5Keep, &md5Data) != 0 ) {
        error_log("requestid(%d) block(%d) Data md5: h0:%X h1:%X h2:%X h3:%X", requestid, blockid,  md5Data.h0, md5Data.h1, md5Data.h2, md5Data.h3);
        return -1;
    }

    return 0;
}

int session_do_read(conn_buf_t *cob, msg_request_t **p_request)
{
    session_info_t *session_info = cob->session_info;

    /* -------- do_read_data:request_header -------- */
    TRACE_LOG_SESSION_COB("Call do_read_data() for request_header.");

    struct msg_request_t request_header;

    do_read_data(cob, &request_header, sizeof(request_header));

    cob = coroutine_self_data();
    /*cob = co_get_data(co_current());*/
    /*cob = pthread_getspecific(key_actived_cob);*/
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
    /*cob = co_get_data(co_current());*/
    /*cob = pthread_getspecific(key_actived_cob);*/
    session_info = cob->session_info;

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
 *              uint32_t file_size;
 *
 *          COMMON_TAIL_FIELD:
 *              uint8_t md5[];
 *              uint8_t data[];
 */

void* session_rx_handler(void *opaque)
{
    UNUSED int ret;
    uint32_t file_size = 0;
    char key[NAME_MAX];

    /*struct session_info_t *session_info = (struct session_info_t*)opaque;*/
    /*struct conn_buf_t *cob = pthread_getspecific(key_actived_cob);*/
    struct conn_buf_t *cob = (struct conn_buf_t*)opaque;
    struct session_info_t *session_info = cob->session_info;

    while ( 1 ){
        /*cob = coroutine_self_data();*/
        /*cob = co_get_data(co_current());*/
        /*trace_log("co_get_data(). cob=%p", cob);*/
        /*session_info = cob->session_info;*/


        /*TRACE_LOG_SESSION_COB("Before session_do_read().");*/
        msg_request_t *request = NULL;
        ret = session_do_read(cob, &request);

        cob = coroutine_self_data();
        /*cob = co_get_data(co_current());*/
        /*cob = pthread_getspecific(key_actived_cob);*/
        session_info = cob->session_info;


        /* XXX */
        if (0)
        {
    /* -------- do_read_data:header -------- */
    TRACE_LOG_SESSION_COB("Call do_read_data() for header.");

    struct msg_request_t request_header;

    do_read_data(cob, &request_header, sizeof(request_header));

    /*cob = coroutine_self_data();*/
    /*cob = co_get_data(co_current());*/
    /*session_info = cob->session_info;*/

    /* -------- check message header -------- */
    if ( !check_msg((msg_header_t*)&request_header) ) {
        WARNING_LOG_SESSION_COB("Check magic_code in message request_header failed!");
        /*TRACE_DATA_TO_FILE("result/header.dat", &request_header, sizeof(request_header));*/

        abort();
        /*session_info->stop = 1;   */
            YIELD_AND_CONTINUE;
    } else {
        /*TRACE_LOG_SESSION_COB("Check magic_code in message request_header OK!");*/
        /*trace_log("request_header.data_length=%d", request_header.data_length);*/
    }

    /* -------- do_read_data:data -------- */
    TRACE_LOG_SESSION_COB("Call do_read_data() for data.");

    request = (struct msg_request_t*)zmalloc(sizeof(request_header) + request_header.data_length);
    memcpy(request, &request_header, sizeof(request_header));

    do_read_data(cob, request->data, request->data_length);
    /*cob = co_get_data(co_current());*/
    /*session_info = cob->session_info;*/
        }
        /* XXX */

        if ( ret != 0 || request == NULL ) {
            YIELD_AND_CONTINUE;
        }


        /*trace_log("fd(%d) block(%d) request->id=%d, request->data_length=%d", session_fd(session_info), cob->blockid, request->id, request->data_length);*/
        /*TRACE_DATA_TO_FILE("result/block.dat", request->data, request->data_length);*/

        if ( 1) {

        /* -------- message args -------- */
        struct msg_arg_t *arg = (struct msg_arg_t*)request->data;
        struct msg_arg_t *argMd5 = NULL;
        if ( request->id == 0 ) {

            session_info->total_writed = 0;

            /* -------- argKey -------- */
            struct msg_arg_t *argKey = arg;

            /* -------- argFileSize -------- */
            struct msg_arg_t *argFileSize = next_arg(argKey);
            file_size = *((uint32_t*)argFileSize->data);

            /*trace_log("\n~~~~~~~~ fd(%d) block(%d) ~~~~~~~~\n Try to open new file. \n key=%s file_size=%d\n", session_fd(session_info), cob->blockid, argKey->data, file_size);*/

            /* -------- argMd5 -------- */
            argMd5 = next_arg(argFileSize);

            /**
             * Open target file for write.
             */
            {
                /* -------- key or uuid for file name -------- */

                if ( argKey->size > 0 ) {
                    uint32_t keyLen = argKey->size < NAME_MAX - 1 ? argKey->size : NAME_MAX - 1;
                    memcpy(key, argKey->data, keyLen);
                    key[keyLen] = '\0';
                } else {
                    uuid_t uuid;
                    uuid_generate(uuid);
                    uuid_unparse(uuid, key);
                }

                /* -------- openfile -------- */
                session_info->f = storage_open_file(&session_info->server_info->storage_info, key, "wb+");
                if ( session_info->f == NULL ){
                    error_log("storage_open_file() failed. key:%s", key);
                    zfree(request);
                    request = NULL;
                    YIELD_AND_CONTINUE;
                }

                total_fopen++;
                trace_log("fd(%d) block(%d) fopen %s.", session_fd(session_info), cob->blockid, key);
            }

        } else {
            /* -------- argMd5 -------- */
            argMd5 = arg;
        }

        struct msg_arg_t *argData = next_arg(argMd5);

        /*debug_log("fd(%d) block(%d) argData: size=%d", session_fd(session_info), cob->blockid, argData->size);*/
        if ( argData->size > 0 ) {

            /**
             * Check data md5 value.
             */

            if ( check_data_md5(request->id, cob->blockid, argMd5, argData) != 0 ){
                error_log("fd(%d) block(%d) Check buffer md5 failed.", session_fd(session_info), cob->blockid);
                /*abort();*/
                zfree(request);
                request = NULL;
                YIELD_AND_CONTINUE;
            }

            /**
             * Do write!
             */

            if ( session_info->f != NULL ){
                uint32_t nowrited_bytes = file_size - session_info->total_writed; 
                assert(argData->size <= nowrited_bytes);

                char *write_buf = argData->data;
                uint32_t write_bytes = argData->size;

                /* is lastest buffer. */
                if ( argData->size == nowrited_bytes ){
                    write_bytes = (argData->size + 511) & ~(511);
                    assert(write_bytes >= argData->size);
                } else {
                    assert(argData->size % 512 == 0);
                    write_bytes = argData->size;
                }
                assert(write_bytes % 512 == 0);

                char aligned_buf[write_bytes] __attribute__((aligned(512)));
                /*posix_memalign((void**)&write_buf, write_bytes, 512);*/
                memcpy(aligned_buf, argData->data, argData->size);
                write_buf = aligned_buf;

                if ( storage_write_file(&session_info->server_info->storage_info, write_buf, write_bytes, session_info->f) < write_bytes ) {
                    error_log("storage_write_file() failed.");
                    zfree(request);
                    request = NULL;
                    YIELD_AND_CONTINUE;
                }

                /*free(write_buf);*/
                session_info->total_writed += write_bytes;
                pthread_yield();

            } else {
                error_log("fd(%d) block(%d) session_info->f == NULL", session_fd(session_info), cob->blockid);
            }
        } 

        zfree(request);
        request = NULL;

        if ( session_info->total_writed >= file_size ){
            /* -------- fclose -------- */
            trace_log("fd(%d) block(%d) fclose %s.", session_fd(session_info), cob->blockid, key);
            total_fclose++;
            storage_close_file(&session_info->server_info->storage_info, session_info->f);
            session_info->f = NULL;
            session_info->total_writed = 0;
            pthread_yield();

            /* -------- response -------- */

            /*session_rx_off(session_info);*/
            /*response_to_client(session_info, RESULT_SUCCESS);*/
            /*session_rx_on(session_info);*/

        }
        }

    }

    return NULL;
}

/* ==================== recv_request() ==================== */ 
/**
 * Running in a thread in recv_queue.
 * One thread per session and many sessions per thread.
 */
/*void recv_request(struct list_head *work_list)*/
void recv_request(void *arg)
{
    /*struct lockfree_queue_t *queue = (struct lockfree_queue_t*)arg;*/
    /*list *queue;*/

    struct conn_buf_t *cob = (struct conn_buf_t*)arg;
    /*UNUSED struct conn_buf_t *n;*/

    
    /* FIXME thread is no safe! */
    /*while ( (cob = lockfree_queue_dequeue(queue)) != NULL ) {*/
    /*list_for_each_entry_safe(cob, n, work_list, rx_block_list) {*/
    /*while (1) {*/
        /*cob = list_entry((work_list)->next, conn_buf_t, rx_block_list);*/
        /*if ( &cob->rx_block_list == work_list ) break;*/

        session_info_t *session_info = cob->session_info;
        server_info_t *server_info = session_info->server_info;

        /*TRACE_LOG_SESSION_COB("Receive request. ");*/

        /*remove_from_recv_queue(session_info, cob); */

        if ( likely( cob->remain_bytes > 0 ) ) {

            /**
             * coroutine enter session_rx_handler() 
             */


            trace_log("co_set_data(). cob=%p", cob);

            coroutine_enter(session_info->rx_co, cob);

            /*co_set_data(session_info->rx_co, cob);*/
            /*co_call(session_info->rx_co);*/

            /*co_setspecific(key_actived_cob, cob);*/
            /*co_resume(session_info->rx_co);*/


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

        destroy_cob(cob);

        /*if ( session_info->f == NULL )*/
            /*response_to_client(session_info, RESULT_SUCCESS);*/

        session_finish_saving_buffer(session_info);

        TRACE_SESSION_INFO("Finish saving a buffer.");
        if ( session_is_waiting(session_info) ){
            TRACE_SESSION_INFO("After session_is_waiting(). ");
            if ( session_info->waiting_for_close == 1 ) { 
                pthread_cond_signal(&session_info->recv_pending_cond);
            }
        }
        pthread_yield();

    /*}*/
}

