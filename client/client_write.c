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
#include "message.h"
#include "legolas.h"
#include "crc32.h"
#include "md5.h"
#include "session.h"
#include "client.h"

static int do_write_block(session_t *session);
void write_file(session_t* session);

/*void on_close(uv_handle_t* handle); [> in client.c <]*/

/**
 * Start next write loop ?
 */
void start_next_write_loop(session_t *session)
{
    client_t *client = CLIENT(session);
    client_args_t *client_args = CLIENT_ARGS(session);

    if ( client_args->total_send < client->total_files ){
        session->id = 0;
        session->total_readed = 0;
        session->connection.total_bytes = 0;

        client_args->file_size = 0;

        trace_log("Start next write_file(). session_id:%d total_send:%d/%d file_opened:%d file_closed:%d", client_args->session_id, client_args->total_send, client->total_files, client_args->file_opened, client_args->file_closed);

        write_file(session);
    } else {
        session->waiting_for_close = 1;
        session_shutdown(session);
        /*uv_close(&session->connection.handle.handle, on_close);*/
    }
}

/* TODO ==================== after_write_response() ==================== */ 
UNUSED static void after_write_response(uv_stream_t *handle, ssize_t nread, const uv_buf_t *buf) 
{
    session_t *session = (session_t*)handle->data;

    if ( nread > 0 ) {
        start_next_write_loop(session);
        /*if ( nread >= sizeof(message_t) ){*/
            /*message_t *msg = (message_t*)buf->base;*/
            /*if ( msg->result == RESULT_SUCCESS ) {*/
                /*start_next_write_loop(session);*/
            /*} else {*/
                /*error_log("Write_file to server failed!");*/
            /*}*/
        /*}*/
    }

}

/* ==================== client_write_alloc() ==================== */ 
UNUSED static void client_write_alloc( uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) 
{
    buf->base = zmalloc(1024);
    buf->len = 1024;
}

/* ==================== client_session_handle_message() ==================== */ 
int client_session_handle_message(session_t *session, message_t *message)
{
    int ret = 0;

    if ( message->msg_type == MSG_TYPE_REQUEST ){
    } else if ( message->msg_type == MSG_TYPE_RESPONSE ) {
        if ( message->result == RESULT_SUCCESS ) {
            start_next_write_loop(session);
        } else {
            warning_log("return message is not SUCCESS.");
            ret = -1;
        }
    }

    return ret;
}

/* ==================== after_write_block() ==================== */ 
static void after_write_block(uv_write_t *write_req, int status) 
{
    session_t *session = (session_t*)write_req->data;
    client_t *client = CLIENT(session);
    client_args_t *client_args = CLIENT_ARGS(session);

    assert(client != NULL);
    zfree(write_req);

    /* Keep write next block? */
    if ( session->total_readed < client_args->file_size ){
        do_write_block(session);
    } else { /* Write finished. */
        /**
         * End the write task.
         */
        trace_log("\n\nsession_id=%d, total_bytes=%d, total_readed=%d", client_args->session_id, session->connection.total_bytes, session->total_readed);

        fclose(client_args->file);
        client_args->file_closed++;

        __sync_add_and_fetch(&client_args->total_send, 1);
        if ( client_args->total_send % 100 == 0 ){
            notice_log("========> End loop %d/%d. session_id:%d key:%s", client_args->total_send, client->total_files, client_args->session_id, client_args->key);
        }

        session_waiting_message(session);
        /*start_next_write_loop(session);*/
        /*int ret = uv_read_start((uv_stream_t*)&session->connection.handle.handle, client_write_alloc, after_write_response);*/
        /*if ( ret != 0 ) { */
            /*session_free(session);*/
            /*error_log("uv_read_start() failed. ret = %d", ret); */
            /*return ; */
        /*}*/
    }
    
}

/* ==================== do_write_block() ==================== */ 
static int do_write_block(session_t *session)
{
    /**
     * Read DEFAULT_CONN_BUF_SIZE bytes into buf from file.
     */

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

    client_args_t *client_args = CLIENT_ARGS(session);

    uint32_t head_size = 0;

    message_t *write_request;

    write_request = alloc_request_message(session->id, MSG_OP_WRITE); 

    head_size += sizeof(message_t);

    /*if ( session->id == 0 ) {*/
        uint32_t keylen = strlen(client_args->key);
        debug_log("client_args->key=%s, keylen=%d", client_args->key, keylen);

        /* -------- key -------- */
        write_request = add_message_arg(write_request, client_args->key, keylen > 128 ? 128 : keylen);
        head_size += sizeof(uint32_t) + keylen;

        /* -------- key_md5 -------- */
        md5_value_t md5Value;
        md5(&md5Value, (const uint8_t*)client_args->key, keylen);
        write_request = add_message_arg(write_request, &md5Value, sizeof(md5_value_t));
        head_size += sizeof(uint32_t) + sizeof(md5_value_t);

        /* -------- file_size -------- */
        uint32_t file_size = client_args->file_size;
        write_request = add_message_arg(write_request, &file_size, sizeof(file_size));
        head_size += sizeof(uint32_t) + sizeof(file_size);

    /*}*/

    /*head_size += sizeof(uint32_t) + sizeof(md5_value_t);*/
    head_size += sizeof(uint32_t) + sizeof(uint32_t); /* CRC32 */

    uint32_t block_size = DEFAULT_SOCKBUF_SIZE - head_size - sizeof(uint32_t); /* data size*/
    block_size -= block_size % 512;

    char buf[block_size]; 
    uint32_t readed = fread(buf, 1, block_size, client_args->file); 
    session->total_readed += readed;

    /* -------- CRC32 -------- */
    uint32_t crc = crc32(0, buf, readed);
    write_request = add_message_arg(write_request, &crc, sizeof(crc));

    /* -------- data -------- */
    write_request = add_message_arg(write_request, (uint8_t *)buf, readed);
    session->id++;
    /*client->write_request = write_request;*/

    /**
     *
     * Write ubuf to server, and set after_wirte_block callback.
     *
     */

    /*TRACE_DATA_TO_FILE("data/client.dat", (char*)write_request, sizeof(msg_header_t) + write_request->data_length);*/

    /* -------- ubuf -------- */
    uint32_t msg_size = sizeof(message_t) + write_request->data_length;
    /*trace_log("msg_size:%d head_size:%d readed:%d body_size:%zu", msg_size, head_size, readed,  head_size + sizeof(uint32_t) + readed); */
    assert(msg_size == head_size + sizeof(uint32_t) + readed);

    session->connection.total_bytes += msg_size;
    uv_buf_t ubuf = uv_buf_init((char *)write_request, msg_size);

    /* -------- write_req -------- */
    uv_write_t *write_req;
    write_req = zmalloc(sizeof(uv_write_t));
    write_req->data = session;

    int r = uv_write(write_req,
            &session->connection.handle.stream,
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
void write_file(session_t* session) 
{
    client_t *client = CLIENT(session);
    client_args_t *client_args = CLIENT_ARGS(session);

    session->connection.total_bytes = 0;

    char file_name[NAME_MAX];
    get_path_file_name(client->filename, file_name, NAME_MAX - 1);

    sprintf(client_args->key, "/test/%s/%04d/%08d-%s", client->key_prefix, client_args->session_id, client_args->total_send, file_name);
    trace_log("write_file(): %s", client_args->key);

    FILE *file = fopen(client->filename, "rb");
    if ( file == NULL ){
        error_log("fopen() failed. file:%s", client->filename);
        return;
    }

    fseek(file, 0, SEEK_END);
    uint32_t file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    client_args->file_size = file_size;
    client_args->file = file;
    client_args->file_opened++;

        /*file_buffer = zmalloc(file_size);*/

        /*fread(file_buffer, 1, file_size, f); */
        /*fclose(f);*/


    do_write_block(session);

}
