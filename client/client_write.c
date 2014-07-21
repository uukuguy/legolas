/**
 * @file   client_write.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-06-06 20:17:40
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

static int do_write_block(client_t *client);
void write_file(client_t* client);

void destroy_client(client_t *client); /* in client.c */
void on_close(uv_handle_t* handle); /* in client.c */

/**
 * Start next write loop ?
 */
void start_next_write_loop(client_t *client)
{
    if ( client->total_send < client->total_files ){
        client->id = 0;
        client->total_readed = 0;
        client->file_size = 0;
        client->connection.total_bytes = 0;

        trace_log("Start next write_file(). total_send:%d/%d", client->total_send, client->total_files);

        write_file(client);
    } else {
        uv_close(&client->connection.handle.handle, on_close);
    }
}

/* TODO ==================== after_write_response() ==================== */ 
static void after_write_response(uv_stream_t *handle, ssize_t nread, const uv_buf_t *buf) 
{
    client_t *client = (client_t*)handle->data;

    /*trace_log("Enter after_write_response().");*/

    start_next_write_loop(client);

    /*pthread_cond_signal(&client->send_pending_cond);*/
}

/* ==================== client_write_alloc() ==================== */ 
static void client_write_alloc( uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) 
{
    buf->base = zmalloc(1024);
    buf->len = 1024;
}

/* ==================== after_write_block() ==================== */ 
static void after_write_block(uv_write_t *write_req, int status) {

    client_t *client = (client_t*)write_req->data;
    assert(client != NULL);
    zfree(write_req);

    /* Keep write next block? */
    if ( client->total_readed < client->file_size ){
        do_write_block(client);
    } else { /* Write finished. */
        /**
         * End the write task.
         */
        trace_log("\n\nclientid=%d, total_bytes=%d, total_readed=%d", client->clientid, client->connection.total_bytes, client->total_readed);

        fclose(client->f);

        __sync_add_and_fetch(&client->total_send, 1);
        if ( client->total_send % 1000 == 0 ){
            notice_log("========> End loop %d/%d. clientid:%d key:%s", client->total_send, client->total_files, client->clientid, client->key);
        }

        /*start_next_write_loop(client);*/

        int ret = uv_read_start((uv_stream_t*)&client->connection.handle.handle, client_write_alloc, after_write_response);
        if ( ret != 0 ) { 
            destroy_client(client);
            error_log("uv_read_start() failed. ret = %d", ret); 
            return ; 
        }
    }
    
}

/* ==================== do_write_block() ==================== */ 
static int do_write_block(client_t *client)
{
    /**
     * Read DEFAULT_CONN_BUF_SIZE bytes into buf from file.
     */

    /*uint32_t block_size = DEFAULT_CONN_BUF_SIZE - 1024;*/
    /*char buf[block_size]; */

    /*uint32_t readed = fread(buf, 1, block_size, client->f); */

    /*uint32_t nRemain = client->file_size - client->total_readed;*/
    /*uint32_t readed = block_size <= nRemain ? block_size : nRemain; */
    /*memcpy(buf, &file_buffer[client->total_readed], readed);*/

    /*client->total_readed += readed;*/

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

    uint32_t head_size = 0;

    msg_request_t *write_request;

    write_request = alloc_request(client->id, MSG_OP_WRITE); 

    /* XXX For debug */
    write_request->reserved = client->total_send;

    head_size += sizeof(msg_request_t);

    /*if ( client->id == 0 ) {*/
        uint32_t keylen = strlen(client->key);
        debug_log("client->key=%s, keylen=%d", client->key, keylen);

        /* -------- key -------- */
        write_request = add_request_arg(write_request, client->key, keylen > 128 ? 128 : keylen);
        head_size += sizeof(uint32_t) + keylen;

        /* -------- key_md5 -------- */
        md5_value_t md5Value;
        md5(&md5Value, (const uint8_t*)client->key, keylen);
        write_request = add_request_arg(write_request, &md5Value, sizeof(md5_value_t));
        head_size += sizeof(uint32_t) + sizeof(md5_value_t);

        /* -------- file_size -------- */
        uint32_t file_size = client->file_size;
        write_request = add_request_arg(write_request, &file_size, sizeof(file_size));
        head_size += sizeof(uint32_t) + sizeof(file_size);

    /*}*/

    /*head_size += sizeof(uint32_t) + sizeof(md5_value_t);*/
    head_size += sizeof(uint32_t) + sizeof(uint32_t); /* CRC32 */

    uint32_t block_size = DEFAULT_CONN_BUF_SIZE - head_size - sizeof(uint32_t); /* data size*/
    block_size -= block_size % 512;

    char buf[block_size]; 
    uint32_t readed = fread(buf, 1, block_size, client->f); 
    client->total_readed += readed;

    /* -------- CRC32 -------- */
    uint32_t crc = crc32(0, buf, readed);
    write_request = add_request_arg(write_request, &crc, sizeof(crc));

    /* -------- data -------- */
    write_request = add_request_arg(write_request, (uint8_t *)buf, readed);
    client->id++;
    /*client->write_request = write_request;*/

    /**
     *
     * Write ubuf to server, and set after_wirte_block callback.
     *
     */

    /*TRACE_DATA_TO_FILE("data/client.dat", (char*)write_request, sizeof(msg_header_t) + write_request->data_length);*/

    /* -------- ubuf -------- */
    uint32_t msg_size = sizeof(msg_request_t) + write_request->data_length;
    assert(msg_size == head_size + sizeof(uint32_t) + readed);

    client->connection.total_bytes += msg_size;
    uv_buf_t ubuf = uv_buf_init((char *)write_request, msg_size);

    /* -------- write_req -------- */
    uv_write_t *write_req;
    write_req = zmalloc(sizeof(uv_write_t));
    write_req->data = client;

    int r = uv_write(write_req,
            &client->connection.handle.stream,
            &ubuf,
            1,
            after_write_block);

    zfree(write_request);

    if ( r != 0 ) {
        error_log("uv_write() failed");
        return r;
    }

    return 0;
}

/* ==================== write_file() ==================== */ 
void write_file(client_t* client) {
    
    client->connection.total_bytes = 0;

    char file_name[NAME_MAX];
    get_path_file_name(client->file, file_name, NAME_MAX - 1);

    sprintf(client->key, "/test/%s/%04d/%08d-%s", client->key_prefix, client->clientid, client->total_send, file_name);
    trace_log("write_file(): %s", client->key);

    FILE *f = fopen(client->file, "rb");
    if ( f == NULL ){
        error_log("fopen() failed. file:%s", client->file);
        return;
    }

    fseek(f, 0, SEEK_END);
    uint32_t file_size = ftell(f);
    fseek(f, 0, SEEK_SET);
    client->file_size = file_size;
    client->f = f;

        /*file_buffer = zmalloc(file_size);*/

        /*fread(file_buffer, 1, file_size, f); */
        /*fclose(f);*/


    do_write_block(client);

}
