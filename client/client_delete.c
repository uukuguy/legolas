/**
 * @file   client_delete.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-07-07 15:30:43
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
#include "client.h"

void delete_file(session_t *session);

/*  ==================== handle_delete_response() ==================== */ 
int handle_delete_response(session_t *session, message_t *response)
{
    assert(response->msg_type == MSG_TYPE_RESPONSE);

    int ret = 0;

    client_t *client = CLIENT(session);
    client_args_t *client_args = CLIENT_ARGS(session);

    message_arg_t *arg = (message_arg_t*)response->data;
    if ( response->result == RESULT_SUCCESS ) {
        /* ---------- key ---------- */
        message_arg_t *argKey = arg;

        if ( client_args->total_recv % 100 == 0 ){
            notice_log("Key Deleted! key=%s", argKey->data);
        }

        if ( client_args->total_recv < client->total_files ){
            session->id = 0;
            session->total_readed = 0;
            session->connection.total_bytes = 0;

            client_args->file_size = 0;

            trace_log("Start next delete_file(). total_recv:%d/%d", client_args->total_recv, client->total_files);

            delete_file(session);
        } else {
            session->waiting_for_close = 1;
            session_shutdown(session);
            /*uv_close(&session->connection.handle.handle, on_close);*/
        }

    } else if ( response->result == RESULT_ERR_NOTFOUND ) {
        /* ---------- key ---------- */
        message_arg_t *argKey = arg;

        warning_log("Not Deleted for NOTFOUND! key=%s", argKey->data);
    } else {
        error_log("Error response code! result=%d", response->result);
        ret = -1;
    }

    return ret;
}

/* ==================== after_delete_request() ==================== */ 
static void after_delete_request(uv_write_t *read_req, int status) 
{
    session_t *session = (session_t*)read_req->data;
    client_t *client = CLIENT(session);
    client_args_t *client_args = CLIENT_ARGS(session);

    assert(client != NULL);
    zfree(read_req);

    __sync_add_and_fetch(&client_args->total_recv, 1);

    if ( client_args->total_recv % 100 == 0 ){
        trace_log("========> End loop %d/%d. clientid:%d key:%s", client_args->total_recv, client->total_files, client_args->session_id, client_args->key);
    }

    session_waiting_message(session);
}

/* ==================== do_delete_request() ==================== */ 
static int do_delete_request(session_t *session)
{
    client_args_t *client_args = CLIENT_ARGS(session);

    uint32_t head_size = 0;

    message_t *read_request = alloc_request_message(MSG_OP_DEL); 
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
    memset(read_req, 0, sizeof(uv_write_t));
    read_req->data = session;

    int r = uv_write(read_req,
            &session->connection.handle.stream,
            &ubuf,
            1,
            after_delete_request);

    zfree(read_request);

    if ( r != 0 ) {
        error_log("uv_write() failed");
        return r;
    }

    return 0;
}

/* ==================== delete_file() ==================== */ 
void delete_file(session_t* session) 
{
    client_t *client = CLIENT(session);
    client_args_t *client_args = CLIENT_ARGS(session);

    session->connection.total_bytes = 0;

    char file_name[NAME_MAX];
    get_path_file_name(client->filename, file_name, NAME_MAX - 1);

    sprintf(client_args->key, "/test/%s/%04d/%08d-%s", client->key_prefix, client_args->session_id, client_args->total_recv, file_name);
    trace_log("delete_file(): %s", client_args->key);

    do_delete_request(session);
}

