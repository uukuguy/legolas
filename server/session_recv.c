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
#include "crc32.h"
/*#include "md5.h"*/
#include "uv.h"
#include "adlist.h"
#include "skiplist.h"
#include "vnode.h"
#include "object.h"
#include "coroutine.h"
#include <pthread.h>
#include <uuid/uuid.h>
#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#include <assert.h>

/*#define STORAGE_LSM */

static uint32_t total_fopen = 0;
static uint32_t total_fclose = 0;

extern UNUSED void response_to_client(session_t *session, enum MSG_RESULT result); /* in session_send.c */

#define YIELD_AND_CONTINUE \
    trace_log("Ready to yield and continue."); \
    coroutine_yield(); \
    cob = coroutine_self_data(); \
    trace_log("After YIELD coroutine_self_data(). cob=%p", cob); \
    session = cob->session; \
    continue; 

typedef struct block_t {
    uint32_t id;

    char key[NAME_MAX];
    md5_value_t key_md5;
    uint32_t file_size;

    char *data;
    uint32_t data_size;
} block_t;

/* ==================== session_write_block() ==================== */ 
int session_write_block(session_t *session, block_t *block)
{
    UNUSED int r = 0;
    char *write_buf = block->data;
    uint32_t write_bytes = block->data_size;

    if ( block->data_size == 0 ) {
        return 0;
    }

    uint32_t nowrited_bytes = block->file_size - session->total_writed; 
    assert(block->data_size <= nowrited_bytes);

    /* TODO Attach to vnode's caching_objects */
#ifdef STORAGE_LSM
    vnode_t *vnode = get_vnode_by_key(session->server, &block->key_md5);
    assert(vnode != NULL);

    object_t obj;
    memcpy(&obj.key_md5, &block->key_md5, sizeof(md5_value_t));
    object_t *object = object_queue_find(vnode->caching_objects, &obj);
    if ( object == NULL ){
        object = object_new(block->key);
        assert(check_md5(&object->key_md5, &block->key_md5) == 0 );
        object->object_size = block->file_size;

        pthread_mutex_lock(&vnode->caching_objects->queue_lock);
        object_queue_insert(vnode->caching_objects, object);
        pthread_mutex_unlock(&vnode->caching_objects->queue_lock);
    }
    slice_t *slice = (slice_t*)zmalloc(sizeof(slice_t));
    slice->seq_num = block->id;

    slice->buf = zmalloc(write_bytes);
    memcpy(slice->buf, write_buf, write_bytes);
    slice->buf_size = write_bytes;
    listAddNodeTail(object->slices, slice);

#else /* #ifdef STORAGE_LSM */

    char *aligned_buf = NULL;

    if ( session->f == NULL ){
        error_log("fd(%d) session->f == NULL", session_fd(session));
        return -1;
    }
    /* is lastest buffer. */
    if ( write_bytes == nowrited_bytes ){
        write_bytes = (block->data_size + 511) & ~(511);
        assert(write_bytes >= block->data_size);
    } else {
        assert(block->data_size % 512 == 0);
        write_bytes = block->data_size;
    }
    assert(write_bytes % 512 == 0);

    r = posix_memalign((void**)&aligned_buf, 512, DEFAULT_CONN_BUF_SIZE);
    if ( r != 0 ){
        error_log("posix_memalign() return %d. msg: %s", r, strerror(r));
        abort();
    }

    memset(aligned_buf, 0, write_bytes);

    assert(write_bytes >= block->data_size);
    memcpy(aligned_buf, block->data, block->data_size);
    write_buf = aligned_buf;


    if ( storage_write_file(&session->server->storage, write_buf, write_bytes, session->f) < write_bytes ) {
        zfree(aligned_buf);
        return -1;
    }
    zfree(aligned_buf);

#endif /* #ifdef STORAGE_LSM */

    session->total_writed += write_bytes;

    pthread_yield();

    return 0;
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

/* ==================== parse_write_request() ==================== */ 
int parse_write_request(session_t *session, msg_request_t *request, block_t *block)
{
    /* -------- message args -------- */
    msg_arg_t *arg = (msg_arg_t*)request->data;
    msg_arg_t *argCRC32 = NULL;
    block->id = request->id;
    /*if ( request->id == 0 ) {*/
        /* -------- argKey -------- */
        msg_arg_t *argKey = arg;
        if ( argKey->size > 0 ) {
            uint32_t keylen = argKey->size < NAME_MAX - 1 ? argKey->size : NAME_MAX - 1;
            memcpy(block->key, argKey->data, keylen);
            block->key[keylen] = '\0';
        } else {
            uuid_t uuid;
            uuid_generate(uuid);
            uuid_unparse(uuid, block->key);
        }

        /* -------- argMd5 -------- */
        msg_arg_t *argMd5 = next_arg(argKey);
        memcpy(&block->key_md5, argMd5->data, sizeof(md5_value_t));

        /* -------- argFileSize -------- */
        msg_arg_t *argFileSize = next_arg(argMd5);
        block->file_size = *((uint32_t*)argFileSize->data);

        /*trace_log("\n~~~~~~~~ fd(%d) block(%d) ~~~~~~~~\n Try to open new file. \n key=%s file_size=%d\n", session_fd(session), cob->blockid, argKey->data, file_size);*/

        /* -------- argCRC32 -------- */
        argCRC32 = next_arg(argFileSize);
    /*} else {*/
        /*argCRC32 = arg;*/
    /*}*/

    msg_arg_t *argData = next_arg(argCRC32);

    /*debug_log("fd(%d) block(%d) argData: size=%d", session_fd(session), cob->blockid, argData->size);*/
    if ( argData->size > 0 ) {
        /**
         * Check data md5 value.
         */

        if ( check_data_crc32(request->id, argCRC32, argData) != 0 ){
            error_log("fd(%d) request_id(%d) Check buffer crc32 failed.", session_fd(session), request->id);
            return -1;
        }
    } 

    block->data = argData->data;
    block->data_size = argData->size;

    return 0;
}

int session_handle_write(session_t *session, msg_request_t *request)
{
    block_t block;

    /** ----------------------------------------
     *    Parse request
     *  ---------------------------------------- */

    if ( parse_write_request(session, request, &block) != 0 ){
        error_log("parse_write_request() failed. key:%s", block.key);
        return -1;
    }
    /*notice_log("block.file_size:%d", block.file_size);*/

    /** ----------------------------------------
     *    Open target file for write if possible.
     *  ---------------------------------------- */

    if ( block.id == 0 ) {

        session->total_writed = 0;

#ifndef STORAGE_LSM
        /*session->f = storage_open_file(&session->server->storage, block.key, "wb+");*/
        session->f = storage_open_file_by_keymd5(&session->server->storage, &block.key_md5, "wb+");
        if ( session->f == NULL ){
            error_log("storage_open_file() failed. key:%s", block.key);
            return -1;
        }
#endif

        total_fopen++;
    }

    /** ----------------------------------------
     *    Write block!
     *  ---------------------------------------- */

    /*trace_log("fd(%d) block(%d) argData: size=%d", session_fd(session), cob->blockid, argData->size);*/
    if ( session_write_block(session, &block) != 0 ) {
        error_log("session_write_block() failed.");
        return -1;
    }


    /** ----------------------------------------
     *    Close storage.
     *  ---------------------------------------- */

    if ( session->total_writed >= block.file_size ){
        /* -------- fclose -------- */
        trace_log("fd(%d) block(%d) storage_file(%p) fclose %s. total_writed:%d file_size:%d", session_fd(session), block.id, session->f, block.key, session->total_writed, block.file_size);
        total_fclose++;

#ifndef STORAGE_LSM
        storage_close_file(&session->server->storage, session->f);
#endif

        session->f = NULL;
        session->total_writed = 0;
        pthread_yield();

        /* -------- response -------- */

        session_rx_off(session);
        response_to_client(session, RESULT_SUCCESS);
        /*session_rx_on(session);*/

    }

    return 0;
}

/* ==================== parse_read_request() ==================== */ 
int parse_read_request(session_t *session, msg_request_t *request, block_t *block)
{
    /* -------- message args -------- */
    msg_arg_t *arg = (msg_arg_t*)request->data;
    block->id = request->id;

    /* -------- argKey -------- */
    msg_arg_t *argKey = arg;
    if ( argKey->size > 0 ) {
        uint32_t keylen = argKey->size < NAME_MAX - 1 ? argKey->size : NAME_MAX - 1;
        memcpy(block->key, argKey->data, keylen);
        block->key[keylen] = '\0';
    } else {
        uuid_t uuid;
        uuid_generate(uuid);
        uuid_unparse(uuid, block->key);
    }

    /* -------- argMd5 -------- */
    msg_arg_t *argMd5 = next_arg(argKey);
    memcpy(&block->key_md5, argMd5->data, sizeof(md5_value_t));

    block->id = 0;
    block->file_size = 0;
    block->data = NULL;
    block->data_size = 0;

    return 0;
}

int session_handle_read(session_t *session, msg_request_t *request)
{
    /** ----------------------------------------
     *    Parse request
     *  ---------------------------------------- */

    block_t block;
    if ( parse_read_request(session, request, &block) != 0 ){
        error_log("parse_read_request() failed. key:%s", block.key);
        return -1;
    }

#ifdef STORAGE_LSM

    vnode_t *vnode = get_vnode_by_key(session->server, &block.key_md5);

    if ( vnode != NULL ) {
        int object_found = 0;
        object_t obj;
        memcpy(&obj.key_md5, &block.key_md5, sizeof(md5_value_t));
        object_t *object = object_queue_find(vnode->caching_objects, &obj);
        if ( object != NULL ) {
            object_found = 1;
        }
        
        if ( object_found ){
            msg_response_t *response = alloc_response(0, RESULT_SUCCESS);

            uint32_t keylen = strlen(block.key);
            response = add_response_arg(response, block.key, keylen);

            response = add_response_arg(response, &object->object_size, sizeof(object->object_size));
            
            uint32_t msg_size = sizeof(msg_response_t) + response->data_length;
            session_send_data(session, (char *)response, msg_size, NULL);

            zfree(response);

        } else {
            msg_response_t *response = alloc_response(0, RESULT_ERR_NOTFOUND);

            warning_log("key:%s NOT FOUND.", block.key);
            uint32_t keylen = strlen(block.key);
            response = add_response_arg(response, block.key, keylen);
            
            uint32_t msg_size = sizeof(msg_response_t) + response->data_length;
            session_send_data(session, (char *)response, msg_size, NULL);

    /*int file = open("object.dat", O_CREAT | O_TRUNC | O_RDWR, 0644);*/
    /*write(file, response, msg_size);*/
    /*close(file);*/

            zfree(response);
        }
    }

#endif

    return 0;
}

int session_handle_delete(session_t *session, msg_request_t *request)
{
    return 0;
}

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
 *              uint32_t file_size;
 *
 *          COMMON_TAIL_FIELD:
 *              uint8_t md5[];
 *              uint8_t data[];
 */

void *session_rx_handler(void *opaque)
{
    UNUSED int ret;

    msg_request_t *request = NULL;
    conn_buf_t *cob = (conn_buf_t*)opaque;
    session_t *session = cob->session;

    while ( 1 ){

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
        }

        zfree(request);
        request = NULL;

        if ( ret != 0 ) {
            YIELD_AND_CONTINUE;
        }
    }

    return NULL;
}

/* ==================== recv_cob_in_queue() ==================== */ 
void recv_cob_in_queue(conn_buf_t *cob)
{
    session_t *session = cob->session;

    if ( likely( cob->remain_bytes > 0 ) ) {

        /**
         * coroutine enter session_rx_handler() 
         */
        coroutine_enter(session->rx_co, cob);

        trace_log("return from co_call.");

        /**
         * too many requests or not ?
         */

        if ( too_many_requests(session) ) {
            session->stop = 1;
        } else {
            session_rx_on(session);
        }

    } else {
        /* -------- remain bytes == 0 -------- */
    }

    delete_cob(cob);

    session_finish_saving_buffer(session);

    if ( session_is_waiting(session) ){
        if ( session->waiting_for_close == 1 ) { 
            pthread_cond_signal(&session->recv_pending_cond);
        }
    }
    pthread_yield();

}

/* ==================== recv_request() ==================== */ 
/**
 * Running in a thread in recv_queue.
 * One thread per session and many sessions per thread.
 */
void recv_request(work_queue_t *wq)
{
    void *nodeData = NULL;
    while ( (nodeData = dequeue_work(wq)) != NULL ){
       recv_cob_in_queue((conn_buf_t*)nodeData);
    }
}

