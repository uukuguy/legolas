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
#include "protocol.h"
#include "legolas.h"
#include "crc32.h"
#include "md5.h"

void destroy_client(client_t *client); /* in client.c */
void on_close(uv_handle_t* handle); /* in client.c */

void delete_file(client_t* client);

/*  ==================== start_next_read_loop() ==================== */ 
void start_next_delete_loop(client_t *client)
{
    if ( client->total_recv < client->total_files ){
        client->id = 0;
        client->total_readed = 0;
        client->file_size = 0;
        client->connection.total_bytes = 0;

        trace_log("Start next delete_file(). total_recv:%d/%d", client->total_recv, client->total_files);

        delete_file(client);
    } else {
        uv_close(&client->connection.handle.handle, on_close);
    }
}

/* ==================== after_read_shutdown() ==================== */ 
/**
 * Called by after_read_response().
 */

UNUSED static void after_delete_shutdown(uv_shutdown_t *shutdown_req, int status) 
{
    uv_close((uv_handle_t*)shutdown_req->handle, on_close);

    zfree(shutdown_req);
}

/*  ==================== after_delete_response() ==================== */ 
static void after_delete_response(uv_stream_t *handle, ssize_t nread, const uv_buf_t *buf) 
{
    client_t *client = (client_t*)handle->data;

    msg_response_t *response = (msg_response_t*)buf->base; 
    msg_arg_t *arg = (msg_arg_t*)response->data;
    msg_arg_t *argKey = arg;

    /*int file = open("client.dat", O_CREAT | O_TRUNC | O_RDWR, 0644);*/
    /*write(file, buf->base, nread);*/
    /*close(file);*/

    trace_log("nread:%zu result: %d", nread, response->result);

    if ( response->result == RESULT_SUCCESS ) {
        msg_arg_t *argObjectSize = next_arg(argKey);
        uint32_t object_size = *(uint32_t*)argObjectSize->data;
        notice_log("Deleted! key=%s object_size=%d", argKey->data, object_size);

    } else if ( response->result == RESULT_ERR_NOTFOUND ) {
        warning_log("Not Deleted for NOTFOUND! key=%s", argKey->data);
    }

    zfree(buf->base);


    start_next_delete_loop(client);

    /*pthread_cond_signal(&client->recv_pending_cond);*/
}

/* ==================== client_read_alloc() ==================== */ 
static void client_delete_alloc( uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) 
{
    buf->base = zmalloc(64 * 1024);
    buf->len = 64 * 1024;

    /* -------- cob -------- */
    /*uint32_t buf_len = sizeof(conn_buf_t);*/
    /*conn_buf_t *cob = (conn_buf_t *)zmalloc(buf_len);*/
    /*init_cob(cob);*/

    /* -------- set uv_buf -------- */
    /*buf->base = cob->base;*/
    /*buf->len = cob->len;*/
}

/* ==================== after_delete_block() ==================== */ 
static void after_delete_block(uv_write_t *read_req, int status) {

    client_t *client = (client_t*)read_req->data;
    assert(client != NULL);
    zfree(read_req);

    __sync_add_and_fetch(&client->total_recv, 1);
    trace_log("========> End loop %d/%d. clientid:%d key:%s", client->total_recv, client->total_files, client->clientid, client->key);

    int ret = uv_read_start((uv_stream_t*)&client->connection.handle.handle, client_delete_alloc, after_delete_response);
    if ( ret != 0 ) { 
        destroy_client(client);
        error_log("uv_read_start() failed. ret = %d", ret); 
        return ; 
    }
    
}

/* ==================== do_delet_block() ==================== */ 
static int do_delete_block(client_t *client)
{
    uint32_t head_size = 0;

    msg_request_t *read_request = alloc_request(client->id, MSG_OP_DEL); 
    head_size += sizeof(msg_request_t);

    uint32_t keylen = strlen(client->key);
    debug_log("client->key=%s, keylen=%d", client->key, keylen);

    /* -------- key -------- */
    read_request = add_request_arg(read_request, client->key, keylen > 128 ? 128 : keylen);
    head_size += sizeof(uint32_t) + keylen;

    /* -------- key_md5 -------- */
    md5_value_t md5Value;
    md5(&md5Value, (const uint8_t*)client->key, keylen);
    read_request = add_request_arg(read_request, &md5Value, sizeof(md5_value_t));
    head_size += sizeof(uint32_t) + sizeof(md5_value_t);

    /*client->read_request = read_request;*/

    /* -------- ubuf -------- */
    uint32_t msg_size = sizeof(msg_request_t) + read_request->data_length;
    assert(msg_size == head_size);

    client->connection.total_bytes += msg_size;
    uv_buf_t ubuf = uv_buf_init((char *)read_request, msg_size);

    /* -------- read_req -------- */
    uv_write_t *read_req;
    read_req = zmalloc(sizeof(uv_write_t));
    read_req->data = client;

    int r = uv_write(read_req,
            &client->connection.handle.stream,
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
void delete_file(client_t* client) 
{
    client->connection.total_bytes = 0;

    char file_name[NAME_MAX];
    get_path_file_name(client->file, file_name, NAME_MAX - 1);

    sprintf(client->key, "/test/%s/%04d/%08d-%s", client->key_prefix, client->clientid, client->total_recv, file_name);
    trace_log("delete_file(): %s", client->key);

    do_delete_block(client);
}

