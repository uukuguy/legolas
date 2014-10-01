/**
 * @file   udb_read_data.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-09-09 14:55:00
 * 
 * @brief  
 * 
 * 
 */

#include "uv.h"
#include "common.h"
#include "work.h"
#include "filesystem.h"
#include "logger.h"
#include "message.h"
#include "crc32.h"
#include "md5.h"
#include "session.h"
#include "udb.h"


/*  ==================== handle_read_response() ==================== */ 
int udb_handle_read_response(session_t *session, message_t *response)
{
    assert(response->msg_type == MSG_TYPE_RESPONSE);

    int ret = 0;

    UNUSED udb_t *udb = udb(session);

    message_arg_t *arg = (message_arg_t*)response->data;

    if ( response->result == RESULT_SUCCESS ) {
        /* ---------- key ---------- */
        message_arg_t *argKey = arg;
        /* ---------- seq_num ---------- */
        message_arg_t *arg_seq_num = message_next_arg(argKey);
        uint32_t seq_num = *(uint32_t*)arg_seq_num->data;
        udb->slice_idx = seq_num;

        /* ---------- nslices ---------- */
        message_arg_t *arg_nslices = message_next_arg(arg_seq_num);
        uint32_t nslices = *(uint32_t*)arg_nslices->data;
        udb->nslices = nslices;

        /* ---------- object_size ---------- */
        message_arg_t *argObjectSize = message_next_arg(arg_nslices);
        uint32_t object_size = *(uint32_t*)argObjectSize->data;
        udb->object_size = object_size;

        /* ---------- data ---------- */
        message_arg_t *arg_data = message_next_arg(argObjectSize);
        UNUSED char *data = arg_data->data;
        uint32_t data_size = arg_data->size;

        udb->total_readed += data_size;

        if ( udb->after_read_object_slice != NULL ){
            msgidx_t *msgidx = zmalloc(sizeof(msgidx_t));
            memset(msgidx, 0, sizeof(msgidx_t));
            msgidx->message = response;

            msgidx->object_size = udb->object_size;
            msgidx->slice_idx = udb->slice_idx;
            msgidx->nslices = udb->nslices;

            msgidx->key = argKey->data;
            msgidx->keylen = argKey->size;

            msgidx->data = arg_data->data;
            msgidx->data_size = arg_data->size;

            udb->after_read_object_slice(udb, msgidx);

            zfree(msgidx);
        }

        if ( udb->total_readed >= object_size ){
            if ( udb->after_read_finished != NULL ){
                udb->after_read_finished(udb, response);
            }
        }

    } else if ( response->result == RESULT_ERR_NOTFOUND ) {
        /* ---------- key ---------- */
        message_arg_t *argKey = arg;
        warning_log("NOT FOUND! key=%s", argKey->data);
        if ( udb->after_read_object_slice != NULL ){
            msgidx_t *msgidx = zmalloc(sizeof(msgidx_t));
            memset(msgidx, 0, sizeof(msgidx_t));
            msgidx->message = response;

            msgidx->key = argKey->data;
            msgidx->keylen = argKey->size;

            udb->after_read_object_slice(udb, msgidx);

            zfree(msgidx);
        }
        ret = 0;
    } else {
        /* ---------- key ---------- */
        message_arg_t *argKey = arg;
        error_log("Error response code! result=%d key=%s", response->result, argKey->data);
        if ( udb->after_read_object_slice != NULL ){
            msgidx_t *msgidx = zmalloc(sizeof(msgidx_t));
            memset(msgidx, 0, sizeof(msgidx_t));
            msgidx->message = response;

            msgidx->key = argKey->data;
            msgidx->keylen = argKey->size;
            
            udb->after_read_object_slice(udb, msgidx);

            zfree(msgidx);
        }
        ret = -1;
    }

    return ret;
}

/* ==================== after_read_request() ==================== */ 
static void after_read_request(uv_write_t *read_req, int status) 
{
    session_t *session = (session_t*)read_req->data;

    udb_t *udb = udb(session);
    assert(udb != NULL);

    zfree(read_req);

    session_waiting_message(session);

}

/* ==================== do_read_request() ==================== */ 
int do_read_request(session_t *session)
{
    udb_t *udb = udb(session);

    uint32_t head_size = 0;

    message_t *read_request = alloc_request_message(MSG_OP_READ); 
    head_size += sizeof(message_t);

    const char *key = udb->key;
    uint32_t keylen = udb->keylen;
    debug_log("udb->key=%s, keylen=%d", key, keylen);

    /* -------- key -------- */
    read_request = add_message_arg(read_request, key, keylen > 128 ? 128 : keylen);
    head_size += sizeof(uint32_t) + keylen;

    /* -------- key_md5 -------- */
    md5_value_t md5Value;
    md5(&md5Value, (const uint8_t*)key, keylen);
    read_request = add_message_arg(read_request, &md5Value, sizeof(md5_value_t));
    head_size += sizeof(uint32_t) + sizeof(md5_value_t);

    /* -------- ubuf -------- */
    uint32_t msg_size = sizeof(message_t) + read_request->data_length;
    assert(msg_size == head_size);

    session->connection.total_bytes += msg_size;
    uv_buf_t ubuf = uv_buf_init((char *)read_request, msg_size);

    /* -------- read_req -------- */
    uv_write_t *read_req;
    read_req = zmalloc(sizeof(uv_write_t));
    memset(read_req, 0, sizeof(uv_write_t));
    read_req->data = session;

    int r = uv_write(read_req,
            &session->connection.handle.stream,
            &ubuf,
            1,
            after_read_request);

    zfree(read_request);

    if ( r != 0 ) {
        error_log("uv_write() failed");
        return r;
    }

    return 0;
}


/* ==================== udb_read_data() ==================== */ 
int udb_read_data(udb_t *udb, int handle, 
        after_read_object_slice_cb after_read_object_slice,
        after_read_finished_cb after_read_finished)
{
    if ( after_read_object_slice != NULL ){
        udb->after_read_object_slice = after_read_object_slice;
    }

    if ( after_read_finished != NULL ){
        udb->after_read_finished = after_read_finished;
    }
    udb->total_readed = 0;

    session_t *session = udb->session;
    
    return do_read_request(session);
}

