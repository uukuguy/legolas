/**
 * @file   udb_write_data.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-09-09 14:23:03
 * 
 * @brief  
 * 
 * 
 */

/*#include "uv.h"*/
#include "common.h"
#include "work.h"
#include "filesystem.h"
#include "logger.h"
#include "message.h"
#include "crc32.h"
#include "md5.h"
#include "session.h"
#include "udb.h"

/* ==================== handle_write_response() ==================== */ 
int udb_handle_write_response(session_t *session, message_t *response)
{
    assert(response->msg_type == MSG_TYPE_RESPONSE);

    /*trace_log("Enter udb_handle_write_response()");*/

    int ret = 0;

    /* FIXME 2014-10-15 04:16:41 */
    /*session_rx_off(session);*/

    if ( response->result != RESULT_SUCCESS ) {
        warning_log("return message is not SUCCESS. result: %d", response->result);
        return -1;
    }

    udb_t *udb = udb(session);
    if ( udb->after_write_finished != NULL ){
        udb->after_write_finished(udb, response);
    }
    udb->finished_works++;
    session->finished_works = udb->finished_works;

    return ret;
}

void client_write_next_file(udb_t *udb); /* in client.c */
/* ==================== udb_after_write_request() ==================== */ 
/*static void udb_after_write_request(uv_write_t *write_req, int status) */
static void udb_after_write_request(session_t *session, int status) 
{
    /*debug_log("Enter udb_after_write_request");*/

    /*session_t *session = (session_t*)write_req->data;*/

    udb_t *udb = udb(session);
    assert(udb!= NULL);

    /*zfree(write_req);*/

    msgidx_t *msgidx = zmalloc(sizeof(msgidx_t));
    memset(msgidx, 0, sizeof(msgidx_t));

    msgidx->object_size = udb->object_size;
    msgidx->slice_idx = udb->slice_idx;
    msgidx->nslices = udb->nslices;

    msgidx->key = udb->key;
    msgidx->keylen = udb->keylen;
    msgidx->data_size = udb->data_size;

    if ( udb->after_write_object_slice != NULL ){

        udb->after_write_object_slice(udb, msgidx);

    }

    assert(udb->total_writed <= udb->object_size);
    if ( udb->total_writed == udb->object_size ){
        if ( udb->after_write_request_done != NULL ){
            udb->after_write_request_done(udb, msgidx);
        }
    }

    zfree(msgidx);
}

/*static uint32_t g_udb_blocks = 0;*/

/* ==================== udb_write_request() ==================== */ 
int udb_write_request(session_t *session, char *data, uint32_t data_size)
{
    /**
     * Read DEFAULT_CONN_BUF_SIZE bytes into buf from file.
     */

    /**
     * Prepare message header.
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

    if ( data_size == 0 ) return 0;

    udb_t *udb = udb(session);

    uint32_t head_size = 0;

    message_t *message = alloc_request_message(MSG_OP_WRITE); 
    head_size += sizeof(message_t);

    const char *key = udb->key;
    uint32_t keylen = udb->keylen;
    /*debug_log("udb->key=%s, keylen=%d", key, keylen);*/

    /* -------- key -------- */
    message = add_message_arg(message, key, keylen > 128 ? 128 : keylen);
    head_size += sizeof(uint32_t) + keylen;

    /* -------- key_md5 -------- */
    md5_value_t md5Value;
    md5(&md5Value, (const uint8_t*)key, keylen);

    message = add_message_arg(message, &md5Value, sizeof(md5_value_t));
    head_size += sizeof(uint32_t) + sizeof(md5_value_t);

    /* -------- object_size -------- */
    uint32_t object_size = udb->object_size;
    message = add_message_arg(message, &object_size, sizeof(object_size));
    head_size += sizeof(uint32_t) + sizeof(object_size);


    /*head_size += sizeof(uint32_t) + sizeof(md5_value_t);*/
    head_size += sizeof(uint32_t) + sizeof(uint32_t); /* CRC32 */

    uint32_t block_size = DEFAULT_SOCKBUF_SIZE - head_size - sizeof(uint32_t); /* data size*/
    block_size = data_size < block_size ? data_size : block_size;
    block_size -= block_size % 512;

    uint32_t writed = block_size;
    if ( block_size + udb->total_writed > udb->object_size ){
        writed = udb->object_size - udb->total_writed;
    }

    /* FIXME */
    char *buf = data;
    /*char *buf = NULL;*/
    /*buf = &client_args->file_data[client_args->file_data_sended];*/
    /*if ( client_args->file_data_sended + block_size > client_args->file_size ){*/
        /*writed = client_args->file_size - client_args->file_data_sended;*/
        /*client_args->file_data_sended += writed;*/
    /*} else {*/
        /*client_args->file_data_sended += block_size;*/
    /*}*/

    udb->total_writed += writed;
    udb->data = data;
    udb->data_size = writed;

    /* -------- CRC32 -------- */
    /* FIXME */
    uint32_t crc = 0;
    /*uint32_t crc = crc32(0, buf, writed);*/
    message = add_message_arg(message, &crc, sizeof(crc));

    /* -------- data -------- */
    message = add_message_arg(message, buf, writed);


    /* -------- ubuf -------- */
    uint32_t msg_size = sizeof(message_t) + message->data_length;
    /*trace_log("msg_size:%d head_size:%d writed:%d body_size:%zu", msg_size, head_size, writed,  head_size + sizeof(uint32_t) + writed); */
    assert(msg_size == head_size + sizeof(uint32_t) + writed);

    message->id = udb->finished_works;

    session->connection.total_bytes += msg_size;

    message->crc32_data = crc32(0, (const char *)message->data, message->data_length);
    session_write_request(session, (char*)message, msg_size, udb_after_write_request);

    zfree(message);


    return writed;
}

/* ==================== udb_append_data() ==================== */ 
int udb_append_data(udb_t *udb, int handle, void *data, uint32_t len)
{
    session_t *session = udb->session;

    return udb_write_request(session, data, len);
}

/* ==================== udb_write_data() ==================== */ 
int udb_write_data(udb_t *udb, int handle, void *data, uint32_t len, 
        after_write_object_slice_cb after_write_object_slice, 
        after_write_finished_cb after_write_finished,
        after_write_request_done_cb after_write_request_done)
{
    if ( after_write_object_slice != NULL ){
        udb->after_write_object_slice = after_write_object_slice;
    }

    if ( after_write_finished != NULL ){
        udb->after_write_finished = after_write_finished;
    }

    if ( after_write_request_done != NULL ){
        udb->after_write_request_done = after_write_request_done;
    }

    udb->total_writed = 0;

    return udb_append_data(udb, handle, data, len);
}

