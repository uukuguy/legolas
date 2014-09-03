/**
 * @file   udclient_read_data.c
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
#include "legolas.h"
#include "crc32.h"
#include "md5.h"
#include "session.h"
#include "udclient.h"


/*  ==================== handle_read_response() ==================== */ 
int udclient_handle_read_response(session_t *session, message_t *response)
{
    assert(response->msg_type == MSG_TYPE_RESPONSE);

    int ret = 0;

    UNUSED udclient_t *udcli = UDCLIENT(session);

    message_arg_t *arg = (message_arg_t*)response->data;

    if ( response->result == RESULT_SUCCESS ) {
        /* ---------- key ---------- */
        message_arg_t *argKey = arg;
        /* ---------- seq_num ---------- */
        message_arg_t *arg_seq_num = message_next_arg(argKey);
        uint32_t seq_num = *(uint32_t*)arg_seq_num->data;
        /* ---------- nslices ---------- */
        message_arg_t *arg_nslices = message_next_arg(arg_seq_num);
        uint32_t nslices = *(uint32_t*)arg_nslices->data;
        /* ---------- object_size ---------- */
        message_arg_t *argObjectSize = message_next_arg(arg_nslices);
        uint32_t object_size = *(uint32_t*)argObjectSize->data;
        /* ---------- data ---------- */
        message_arg_t *arg_data = message_next_arg(argObjectSize);
        UNUSED char *data = arg_data->data;
        uint32_t data_size = arg_data->size;

        session->total_readed += data_size;
        udcli->total_readed += data_size;

        if ( udcli->after_read_object_slice != NULL ){
            msgidx_t *msgidx = zmalloc(sizeof(msgidx_t));
            msgidx->message = response;
            msgidx->object_size = object_size;
            msgidx->slice_idx = seq_num;
            msgidx->nslices = nslices;
            msgidx->key = argKey->data;
            msgidx->keylen = argKey->size;
            msgidx->data = arg_data->data;
            msgidx->data_size = arg_data->size;

            udcli->after_read_object_slice(udcli, msgidx, response);

            zfree(msgidx);
        }

        if ( session->total_readed >= object_size ){
            if ( udcli->after_read_finished != NULL ){
                udcli->after_read_finished(udcli, response);
            }
        }

    } else if ( response->result == RESULT_ERR_NOTFOUND ) {
        /* ---------- key ---------- */
        message_arg_t *argKey = arg;
        warning_log("NOT FOUND! key=%s", argKey->data);
        if ( udcli->after_read_object_slice != NULL ){
            msgidx_t *msgidx = zmalloc(sizeof(msgidx_t));
            msgidx->message = response;
            msgidx->key = argKey->data;

            udcli->after_read_object_slice(udcli, msgidx, response);

            zfree(msgidx);
        }
        ret = 0;
    } else {
        /* ---------- key ---------- */
        message_arg_t *argKey = arg;
        error_log("Error response code! result=%d key=%s", response->result, argKey->data);
        if ( udcli->after_read_object_slice != NULL ){
            msgidx_t *msgidx = zmalloc(sizeof(msgidx_t));
            msgidx->message = response;
            msgidx->key = argKey->data;
            
            udcli->after_read_object_slice(udcli, msgidx, response);

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

    udclient_t *udcli = UDCLIENT(session);
    assert(udcli != NULL);

    zfree(read_req);

    session_waiting_message(session);

}

/* ==================== do_read_request() ==================== */ 
int do_read_request(session_t *session)
{
    udclient_t *udcli = UDCLIENT(session);
    session->total_readed = 0;
    udcli->total_readed = 0;

    uint32_t head_size = 0;

    message_t *read_request = alloc_request_message(session->id, MSG_OP_READ); 
    head_size += sizeof(message_t);

    const char *key = udcli->key;
    uint32_t keylen = udcli->keylen;
    debug_log("udcli->key=%s, keylen=%d", key, keylen);

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


