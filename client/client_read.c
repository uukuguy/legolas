/**
 * @file   client_read.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-06-06 07:16:03
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
#include "client.h"

void read_file(session_t* session);

/*  ==================== handle_read_response() ==================== */ 
int handle_read_response(session_t *session, message_t *response)
{
    assert(response->msg_type == MSG_TYPE_RESPONSE);

    int ret = 0;

    UNUSED client_t *client = CLIENT(session);
    UNUSED client_args_t *client_args = CLIENT_ARGS(session);

    message_arg_t *arg = (message_arg_t*)response->data;

    if ( response->result == RESULT_SUCCESS ) {
        /* ---------- key ---------- */
        message_arg_t *argKey = arg;
        /* ---------- object_size ---------- */
        message_arg_t *argObjectSize = message_next_arg(argKey);
        uint32_t object_size = *(uint32_t*)argObjectSize->data;
        /* ---------- seq_num ---------- */
        message_arg_t *arg_seq_num = message_next_arg(argObjectSize);
        uint32_t seq_num = *(uint32_t*)arg_seq_num->data;
        /* ---------- nslices ---------- */
        message_arg_t *arg_nslices = message_next_arg(arg_seq_num);
        uint32_t nslices = *(uint32_t*)arg_nslices->data;
        /* ---------- data ---------- */
        message_arg_t *arg_data = message_next_arg(arg_nslices);
        UNUSED char *data = arg_data->data;
        uint32_t data_size = arg_data->size;

        if ( session->callbacks.handle_read_response != NULL ){
            msgidx_t *msgidx = zmalloc(sizeof(msgidx_t));
            msgidx->message = response;
            msgidx->object_size = object_size;
            msgidx->slice_idx = seq_num;
            msgidx->nslices = nslices;
            msgidx->key = argKey->data;
            msgidx->keylen = argKey->size;
            msgidx->data = arg_data->data;
            msgidx->data_size = arg_data->size;
            session->callbacks.handle_read_response(session, msgidx);
            zfree(msgidx);
        }

        if ( client_args->total_recv % 100 == 0 ){
            notice_log("Key Found! key=%s object_size=%d seq:%d/%d data_size:%d", argKey->data, object_size, seq_num + 1, nslices, data_size);
        }

    } else if ( response->result == RESULT_ERR_NOTFOUND ) {
        /* ---------- key ---------- */
        message_arg_t *argKey = arg;
        warning_log("NOT FOUND! key=%s", argKey->data);
        if ( session->callbacks.handle_read_response != NULL ){
            msgidx_t *msgidx = zmalloc(sizeof(msgidx_t));
            msgidx->message = response;
            msgidx->key = argKey->data;
            session->callbacks.handle_read_response(session, msgidx);
            zfree(msgidx);
        }
        ret = 0;
    } else {
        /* ---------- key ---------- */
        message_arg_t *argKey = arg;
        error_log("Error response code! result=%d key=%s", response->result, argKey->data);
        if ( session->callbacks.handle_read_response != NULL ){
            msgidx_t *msgidx = zmalloc(sizeof(msgidx_t));
            msgidx->message = response;
            msgidx->key = argKey->data;
            session->callbacks.handle_read_response(session, msgidx);
            zfree(msgidx);
        }
        ret = -1;
    }

    return ret;
}

/*  ==================== client_handle_read_response() ==================== */ 
int client_handle_read_response(session_t *session, message_t *response)
{
    client_t *client = CLIENT(session);
    client_args_t *client_args = CLIENT_ARGS(session);

    int ret;
    ret = handle_read_response(session, response);

    __sync_add_and_fetch(&client_args->total_recv, 1);

    if ( client_args->total_recv < client->total_files ){
        session->id = 0;
        session->total_readed = 0;
        session->connection.total_bytes = 0;

        client_args->file_size = 0;

        trace_log("Start next read_file(). total_recv:%d/%d", client_args->total_recv, client->total_files);

        read_file(session);
    } else {
        session->waiting_for_close = 1;
        session_shutdown(session);
        /*uv_close(&session->connection.handle.handle, on_close);*/
    }

    return ret;
}

/* ==================== after_read_request() ==================== */ 
static void after_read_request(uv_write_t *read_req, int status) 
{
    session_t *session = (session_t*)read_req->data;
    client_t *client = CLIENT(session);
    client_args_t *client_args = CLIENT_ARGS(session);

    assert(client != NULL);
    zfree(read_req);

    /*__sync_add_and_fetch(&client_args->total_recv, 1);*/

    if ( client_args->total_recv % 100 == 0 ){
        trace_log("========> End loop %d/%d. clientid:%d key:%s", client_args->total_recv, client->total_files, client_args->session_id, client_args->key);
    }

    session_waiting_message(session);

}

/* ==================== do_read_request() ==================== */ 
int do_read_request(session_t *session)
{
    client_args_t *client_args = CLIENT_ARGS(session);
    uint32_t head_size = 0;

    message_t *read_request = alloc_request_message(session->id, MSG_OP_READ); 
    head_size += sizeof(message_t);

    uint32_t keylen = strlen(client_args->key);
    debug_log("client_args->key=%s, keylen=%d", client_args->key, keylen);

    /* -------- key -------- */
    read_request = add_message_arg(read_request, client_args->key, keylen > 128 ? 128 : keylen);
    head_size += sizeof(uint32_t) + keylen;

    /* -------- key_md5 -------- */
    md5_value_t md5Value;
    md5(&md5Value, (const uint8_t*)client_args->key, keylen);
    read_request = add_message_arg(read_request, &md5Value, sizeof(md5_value_t));
    head_size += sizeof(uint32_t) + sizeof(md5_value_t);

    /*client->read_request = read_request;*/

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

/* ==================== read_file() ==================== */ 
void read_file(session_t* session) 
{
    client_t *client = CLIENT(session);
    client_args_t *client_args = CLIENT_ARGS(session);

    session->connection.total_bytes = 0;

    char file_name[NAME_MAX];
    get_path_file_name(client->filename, file_name, NAME_MAX - 1);

    sprintf(client_args->key, "/test/%s/%04d/%08d-%s", client->key_prefix, client_args->session_id, client_args->total_recv, file_name);
    trace_log("read_file(): %s", client_args->key);

    do_read_request(session);
}

