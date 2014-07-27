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
#include "legolas.h"
#include "crc32.h"
#include "md5.h"
#include "session.h"
#include "client.h"

/*void on_close(uv_handle_t* handle); [> in client.c <]*/

void delete_file(session_t *session);

/*  ==================== start_next_read_loop() ==================== */ 
void start_next_delete_loop(session_t *session)
{
    client_t *client = CLIENT(session);
    client_args_t *client_args = CLIENT_ARGS(session);

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
}

/*  ==================== after_delete_response() ==================== */ 
static void after_delete_response(uv_stream_t *handle, ssize_t nread, const uv_buf_t *buf) 
{
    session_t *session = (session_t*)handle->data;

    if ( nread > 0 ) {
        message_t *response = (message_t*)buf->base; 
        message_arg_t *arg = (message_arg_t*)response->data;
        message_arg_t *argKey = arg;

        /*int file = open("client.dat", O_CREAT | O_TRUNC | O_RDWR, 0644);*/
        /*write(file, buf->base, nread);*/
        /*close(file);*/

        trace_log("nread:%zu result: %d", nread, response->result);

        if ( response->result == RESULT_SUCCESS ) {
            /*message_arg_t *argObjectSize = message_next_arg(argKey);*/
            /*uint32_t object_size = *(uint32_t*)argObjectSize->data;*/
            /*notice_log("Deleted! key=%s object_size=%d", argKey->data, object_size);*/

        } else if ( response->result == RESULT_ERR_NOTFOUND ) {
            warning_log("Not Deleted for NOTFOUND! key=%s", argKey->data);
        }

        zfree(buf->base);


        start_next_delete_loop(session);

        /*pthread_cond_signal(&client->recv_pending_cond);*/
    }
}

/* ==================== client_read_alloc() ==================== */ 
static void client_delete_alloc( uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) 
{
    buf->base = zmalloc(64 * 1024);
    buf->len = 64 * 1024;

    /* -------- sockbuf -------- */
    /*uint32_t buf_len = sizeof(sockbuf_t);*/
    /*sockbuf_t *sockbuf = (sockbuf_t *)zmalloc(buf_len);*/
    /*sockbuf_init(sockbuf);*/

    /* -------- set uv_buf -------- */
    /*buf->base = sockbuf->base;*/
    /*buf->len = sockbuf->len;*/
}

/* ==================== after_delete_block() ==================== */ 
static void after_delete_block(uv_write_t *read_req, int status) 
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

    int ret = uv_read_start((uv_stream_t*)&session->connection.handle.handle, client_delete_alloc, after_delete_response);
    if ( ret != 0 ) { 
        session_free(session);
        error_log("uv_read_start() failed. ret = %d", ret); 
        return ; 
    }
    
}

/* ==================== do_delet_block() ==================== */ 
static int do_delete_block(session_t *session)
{
    client_args_t *client_args = CLIENT_ARGS(session);

    uint32_t head_size = 0;

    message_t *read_request = alloc_request_message(session->id, MSG_OP_DEL); 
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
            after_delete_block);

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

    do_delete_block(session);
}

