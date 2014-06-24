/**
 * @file   client.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-05-05 20:54:54
 * 
 * @brief  
 * 
 * 
 */

#include "uv.h"
#include "common.h"
#include "work.h"
#include "logger.h"
#include "protocol.h"
#include "legolas.h"
#include "crc32.h"
/*#include "md5.h"*/


static void after_write_block(uv_write_t *req, int status);
static void write_file(client_t* client);

/* ==================== destroy_client() ==================== */ 
void destroy_client(client_t *client){
    assert(client != NULL);
    pthread_mutex_destroy(&client->send_pending_lock);
    pthread_cond_destroy(&client->send_pending_cond);
    zfree(client);
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

    if ( client->id == 0 ) {
        uint32_t file_size = client->file_size;
        uint32_t keylen = strlen(client->key);
        debug_log("client->key=%s, keylen=%d", client->key, keylen);
        /* -------- key -------- */
        write_request = add_request_arg(write_request, client->key, keylen > 128 ? 128 : keylen);
        head_size += sizeof(uint32_t) + keylen;
        /* -------- file_size -------- */
        write_request = add_request_arg(write_request, &file_size, sizeof(file_size));
        head_size += sizeof(uint32_t) + sizeof(file_size);
    }

    /*head_size += sizeof(uint32_t) + sizeof(md5_value_t);*/
    head_size += sizeof(uint32_t) + sizeof(uint32_t); /* CRC32 */

    uint32_t block_size = DEFAULT_CONN_BUF_SIZE - head_size - sizeof(uint32_t); /* data size*/
    block_size -= block_size % 512;

    char buf[block_size]; 
    uint32_t readed = fread(buf, 1, block_size, client->f); 
    client->total_readed += readed;

    /* -------- md5 -------- */
    /*md5_value_t md5_value;*/
    /*md5(&md5_value, (uint8_t *)buf, readed);*/
    /*write_request = add_request_arg(write_request, &md5_value, sizeof(md5_value_t));*/

    /* -------- CRC32 -------- */
    uint32_t crc = crc32(0, buf, readed);
    write_request = add_request_arg(write_request, &crc, sizeof(crc));

    /* -------- data -------- */
    write_request = add_request_arg(write_request, (uint8_t *)buf, readed);
    client->id++;
    client->write_request = write_request;

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
    if ( r != 0 ) {
        error_log("uv_write() failed");
        return r;
    }

    return 0;
}

/* TODO ==================== client_alloc() ==================== */ 
UNUSED static void client_alloc( uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf);

static void client_alloc( uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) 
{
    buf->base = zmalloc(1024);
    buf->len = 1024;
}

/* ==================== on_close() ==================== */ 
static void on_close(uv_handle_t* handle) {
    notice_log("on_close()");

    client_t *client = (client_t*)handle->data;

    destroy_client(client);
    handle->data = NULL;
}

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

/* TODO ==================== after_response() ==================== */ 
UNUSED static void after_response(uv_stream_t *handle, ssize_t nread, const uv_buf_t *buf); 

static void after_response(uv_stream_t *handle, ssize_t nread, const uv_buf_t *buf) 
{
    client_t *client = (client_t*)handle->data;

    notice_log("Enter after_response().");

    start_next_write_loop(client);

    pthread_cond_signal(&client->send_pending_cond);
}

/* ==================== after_write_block() ==================== */ 
static void after_write_block(uv_write_t *write_req, int status) {

    client_t *client = (client_t*)write_req->data;
    assert(client != NULL);
    zfree(write_req);
    if ( client->write_request != NULL ){
        zfree(client->write_request);
        client->write_request = NULL;
    }

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
        notice_log("========> End loop %d/%d. clientid:%d <========", client->total_send, client->total_files, client->clientid);

        /*start_next_write_loop(client);*/

        int ret = uv_read_start((uv_stream_t*)&client->connection.handle.handle, client_alloc, after_response);
        if ( ret != 0 ) { 
            destroy_client(client);
            error_log("uv_read_start() failed. ret = %d", ret); 
            return ; 
        }
    }
    
}

/* ==================== write_file() ==================== */ 
static void write_file(client_t* client) {
    
    client->connection.total_bytes = 0;

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

/* ==================== on_connect() ==================== */ 
static void on_connect(uv_connect_t *req, int status) {
  client_t *client = (client_t*)req->handle->data;

  notice_log("Connected to server %s:%d", client->server, client->port);

  write_file(client);
}

/* ==================== start_connect() ==================== */ 
int start_connect(int clientid, const char *server, int port, int op_code, const char *key, const char *file, int total_files) 
{
    notice_log("Enter start_connect(). clientid:%d server=%s, port=%d", clientid, server, port);

    int r;

    /* -------- loop -------- */
    uv_loop_t client_loop;
    uv_loop_init(&client_loop);
    uv_loop_t *loop = &client_loop;

    trace_log("loop created.");

    /* -------- server_addr -------- */
    struct sockaddr_in server_addr;
    r = uv_ip4_addr(server, port, &server_addr);
    if ( r ) {
        error_log("uv_ip4_addr() failed.");
        return -1;
    }

    /* -------- client -------- */
    client_t *client = (client_t*)zmalloc(sizeof(client_t));
    client->clientid = clientid;
    client->total_send = 0;
    client->server = server;
    client->port = port;
    client->key = key;
    client->file = file;
    client->total_files = total_files;
    client->write_request = NULL;


    client->id = 0;
    client->total_readed = 0;
    client->file_size = 0;
    client->connection.total_bytes = 0;

    pthread_mutex_init(&client->send_pending_lock, NULL);
    pthread_cond_init(&client->send_pending_cond, NULL);

    /* -------- tcp_handle -------- */
    uv_tcp_t *tcp_handle = &client->connection.handle.tcp;

    /* -------- uv_tcp_init -------- */
    r = uv_tcp_init(loop, tcp_handle);
    if ( r ) {
        error_log("uv_tcp_init() failed.");
        destroy_client(client);
        return -1;
    }
    tcp_handle->data = (void*)client; 

    /* -------- uv_tcp_connect -------- */
    r = uv_tcp_connect(&client->connect_req,
            tcp_handle,
            (const struct sockaddr*) &server_addr,
            on_connect);
    if ( r ) {
        error_log("uv_tcp_connect() failed.");
        destroy_client(client);
        return -1;
    }

    /* -------- uv_run -------- */
    r = uv_run(loop, UV_RUN_DEFAULT);
    if ( r ) {
        error_log("uv_run() failed.");
        destroy_client(client);
        return -1;
    }

    /* FIXME */
    /*MAKE_VALGRIND_HAPPY(loop);*/

    /*close_loop(loop);      */
    /*uv_loop_delete(loop);  */

    notice_log("Client %d exit.", clientid);

    return 0;
}

