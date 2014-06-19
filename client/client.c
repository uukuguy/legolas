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
#include "md5.h"


static void after_write_block(uv_write_t *req, int status);
static void write_file(struct client_info_t* client_info);

/* ==================== destroy_client() ==================== */ 
void destroy_client(client_info_t *client_info){
    assert(client_info != NULL);
    pthread_mutex_destroy(&client_info->send_pending_lock);
    pthread_cond_destroy(&client_info->send_pending_cond);
    zfree(client_info);
}

/* ==================== do_write_block() ==================== */ 
static int do_write_block(struct client_info_t *client_info)
{
    /**
     * Read DEFAULT_CONN_BUF_SIZE bytes into buf from file.
     */

    /*uint32_t block_size = DEFAULT_CONN_BUF_SIZE - 1024;*/
    /*char buf[block_size]; */

    /*uint32_t readed = fread(buf, 1, block_size, client_info->f); */

    /*uint32_t nRemain = client_info->file_size - client_info->total_readed;*/
    /*uint32_t readed = block_size <= nRemain ? block_size : nRemain; */
    /*memcpy(buf, &file_buffer[client_info->total_readed], readed);*/

    /*client_info->total_readed += readed;*/

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

    struct msg_request_t *write_request;

    write_request = alloc_request(client_info->id, MSG_OP_WRITE); 

    /* XXX For debug */
    write_request->reserved = client_info->total_send;

    head_size += sizeof(struct msg_request_t);

    if ( client_info->id == 0 ) {
        uint32_t file_size = client_info->file_size;
        uint32_t keylen = strlen(client_info->key);
        debug_log("client_info->key=%s, keylen=%d", client_info->key, keylen);
        /* -------- key -------- */
        write_request = add_request_arg(write_request, client_info->key, keylen > 128 ? 128 : keylen);
        head_size += sizeof(uint32_t) + keylen;
        /* -------- file_size -------- */
        write_request = add_request_arg(write_request, &file_size, sizeof(file_size));
        head_size += sizeof(uint32_t) + sizeof(file_size);
    }

    head_size += sizeof(uint32_t) + sizeof(md5_value_t);

    uint32_t block_size = DEFAULT_CONN_BUF_SIZE - head_size - sizeof(uint32_t); /* data size*/
    block_size -= block_size % 512;

    char buf[block_size]; 
    uint32_t readed = fread(buf, 1, block_size, client_info->f); 
    client_info->total_readed += readed;

    /* -------- md5 -------- */
    /*struct md5_value_t md5_value;*/
    /*md5(&md5_value, (uint8_t *)buf, readed);*/
    /*write_request = add_request_arg(write_request, &md5_value, sizeof(md5_value_t));*/

    /* -------- CRC32 -------- */
    uint32_t crc = crc32(0, buf, readed);
    write_request = add_request_arg(write_request, &crc, sizeof(crc));

    /* -------- data -------- */
    write_request = add_request_arg(write_request, (uint8_t *)buf, readed);
    client_info->id++;
    client_info->write_request = write_request;

    /**
     *
     * Write ubuf to server, and set after_wirte_block callback.
     *
     */

    /*TRACE_DATA_TO_FILE("data/client.dat", (char*)write_request, sizeof(msg_header_t) + write_request->data_length);*/

    /* -------- ubuf -------- */
    uint32_t msg_size = sizeof(msg_request_t) + write_request->data_length;
    assert(msg_size == head_size + sizeof(uint32_t) + readed);

    client_info->connection.total_bytes += msg_size;
    uv_buf_t ubuf = uv_buf_init((char *)write_request, msg_size);

    /* -------- write_req -------- */
    uv_write_t *write_req;
    write_req = zmalloc(sizeof(uv_write_t));
    write_req->data = client_info;

    int r = uv_write(write_req,
            &client_info->connection.handle.stream,
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

    struct client_info_t *client_info = (struct client_info_t*)handle->data;

    destroy_client(client_info);
    handle->data = NULL;
}

/**
 * Start next write loop ?
 */
void start_next_write_loop(client_info_t *client_info)
{
    if ( client_info->total_send < client_info->total_files ){
        client_info->id = 0;
        client_info->total_readed = 0;
        client_info->file_size = 0;
        client_info->connection.total_bytes = 0;

        trace_log("Start next write_file(). total_send:%d/%d", client_info->total_send, client_info->total_files);

        write_file(client_info);
    } else {
        uv_close(&client_info->connection.handle.handle, on_close);
    }
}

/* TODO ==================== after_response() ==================== */ 
UNUSED static void after_response(uv_stream_t *handle, ssize_t nread, const uv_buf_t *buf); 

static void after_response(uv_stream_t *handle, ssize_t nread, const uv_buf_t *buf) 
{
    client_info_t *client_info = (struct client_info_t*)handle->data;

    notice_log("Enter after_response().");

    start_next_write_loop(client_info);

    pthread_cond_signal(&client_info->send_pending_cond);
}

/* ==================== after_write_block() ==================== */ 
static void after_write_block(uv_write_t *write_req, int status) {

    struct client_info_t *client_info = (struct client_info_t*)write_req->data;
    assert(client_info != NULL);
    zfree(write_req);
    if ( client_info->write_request != NULL ){
        zfree(client_info->write_request);
        client_info->write_request = NULL;
    }

    /* Keep write next block? */
    if ( client_info->total_readed < client_info->file_size ){
        do_write_block(client_info);
    } else { /* Write finished. */
        /**
         * End the write task.
         */
        trace_log("\n\nclientid=%d, total_bytes=%d, total_readed=%d", client_info->clientid, client_info->connection.total_bytes, client_info->total_readed);

        fclose(client_info->f);

        __sync_add_and_fetch(&client_info->total_send, 1);
        notice_log("========> End loop %d/%d. clientid:%d <========", client_info->total_send, client_info->total_files, client_info->clientid);

        /*start_next_write_loop(client_info);*/

        int ret = uv_read_start((uv_stream_t*)&client_info->connection.handle.handle, client_alloc, after_response);
        if ( ret != 0 ) { 
            destroy_client(client_info);
            error_log("uv_read_start() failed. ret = %d", ret); 
            return ; 
        }
    }
    
}

/* ==================== write_file() ==================== */ 
static void write_file(struct client_info_t* client_info) {
    
    client_info->connection.total_bytes = 0;

    FILE *f = fopen(client_info->file, "rb");
    if ( f == NULL ){
        error_log("fopen() failed. file:%s", client_info->file);
        return;
    }

    fseek(f, 0, SEEK_END);
    uint32_t file_size = ftell(f);
    fseek(f, 0, SEEK_SET);
    client_info->file_size = file_size;
    client_info->f = f;

        /*file_buffer = zmalloc(file_size);*/

        /*fread(file_buffer, 1, file_size, f); */
        /*fclose(f);*/


    do_write_block(client_info);

}

/* ==================== on_connect() ==================== */ 
static void on_connect(uv_connect_t *req, int status) {
  struct client_info_t *client_info = (struct client_info_t*)req->handle->data;

  notice_log("Connected to server %s:%d", client_info->server, client_info->port);

  write_file(client_info);
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

    /* -------- client_info -------- */
    struct client_info_t *client_info = (struct client_info_t*)zmalloc(sizeof(client_info_t));
    client_info->clientid = clientid;
    client_info->total_send = 0;
    client_info->server = server;
    client_info->port = port;
    client_info->key = key;
    client_info->file = file;
    client_info->total_files = total_files;
    client_info->write_request = NULL;


    client_info->id = 0;
    client_info->total_readed = 0;
    client_info->file_size = 0;
    client_info->connection.total_bytes = 0;

    pthread_mutex_init(&client_info->send_pending_lock, NULL);
    pthread_cond_init(&client_info->send_pending_cond, NULL);

    /* -------- tcp_handle -------- */
    uv_tcp_t *tcp_handle = &client_info->connection.handle.tcp;

    /* -------- uv_tcp_init -------- */
    r = uv_tcp_init(loop, tcp_handle);
    if ( r ) {
        error_log("uv_tcp_init() failed.");
        destroy_client(client_info);
        return -1;
    }
    tcp_handle->data = (void*)client_info; 

    /* -------- uv_tcp_connect -------- */
    r = uv_tcp_connect(&client_info->connect_req,
            tcp_handle,
            (const struct sockaddr*) &server_addr,
            on_connect);
    if ( r ) {
        error_log("uv_tcp_connect() failed.");
        destroy_client(client_info);
        return -1;
    }

    /* -------- uv_run -------- */
    r = uv_run(loop, UV_RUN_DEFAULT);
    if ( r ) {
        error_log("uv_run() failed.");
        destroy_client(client_info);
        return -1;
    }

    /* FIXME */
    /*MAKE_VALGRIND_HAPPY(loop);*/

    /*close_loop(loop);      */
    /*uv_loop_delete(loop);  */

    notice_log("Client %d exit.", clientid);

    return 0;
}

