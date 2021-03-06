/**
 * @file   udb_delete_data.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-09-09 14:55:27
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


/*  ==================== udb_handle_delete_response() ==================== */ 
int udb_handle_delete_response(session_t *session, message_t *response)
{
    assert(response->msg_type == MSG_TYPE_RESPONSE);

    int ret = 0;

    udb_t *udb = udb(session);

    message_arg_t *arg = (message_arg_t*)response->data;

    if ( response->result == RESULT_SUCCESS ) {

    } else if ( response->result == RESULT_ERR_NOTFOUND ) {
        /* ---------- key ---------- */
        message_arg_t *argKey = arg;

        warning_log("Not Deleted for NOTFOUND! key=%s", argKey->data);
    } else {
        error_log("Error response code! result=%d", response->result);
        ret = -1;
    }

    if ( udb->after_delete_finished != NULL ){
        udb->after_delete_finished(udb, response);
    }

    udb->finished_works++;

    return ret;
}

/* ==================== udb_after_delete_request() ==================== */ 
/*static void udb_after_delete_request(uv_write_t *read_req, int status) */
static void udb_after_delete_request(session_t *session, int status) 
{
    /*session_t *session = (session_t*)read_req->data;*/

    udb_t *udb = udb(session);
    assert(udb != NULL);

    /*zfree(read_req);*/

    session_waiting_message(session);
}

/* ==================== udb_delete_request() ==================== */ 
int udb_delete_request(session_t *session)
{
    udb_t *udb = udb(session);

    uint32_t head_size = 0;

    message_t *message = alloc_request_message(MSG_OP_DEL); 
    head_size += sizeof(message_t);

    const char *key = udb->key;
    uint32_t keylen = udb->keylen;
    debug_log("client_args->key=%s, keylen=%d", key, keylen);

    /* -------- key -------- */
    message = add_message_arg(message, key, keylen > 128 ? 128 : keylen);
    head_size += sizeof(uint32_t) + keylen;

    /* -------- key_md5 -------- */
    md5_value_t md5Value;
    md5(&md5Value, (const uint8_t*)key, keylen);
    message = add_message_arg(message, &md5Value, sizeof(md5_value_t));
    head_size += sizeof(uint32_t) + sizeof(md5_value_t);

    /* -------- ubuf -------- */
    uint32_t msg_size = sizeof(message_t) + message->data_length;
    assert(msg_size == head_size);

    session->connection.total_bytes += msg_size;

    message->crc32_data = crc32(0, (const char *)message->data, message->data_length);
    int r = session_write_request(session, (char*)message, msg_size, udb_after_delete_request);
    zfree(message);

    return r;
}

/* ==================== udb_delete_data() ==================== */ 
int udb_delete_data(udb_t *udb, after_delete_finished_cb after_delete_finished)
{
    if ( after_delete_finished != NULL ){
        udb->after_delete_finished = after_delete_finished;
    }

    session_t *session = udb->session;

    return udb_delete_request(session);
}

