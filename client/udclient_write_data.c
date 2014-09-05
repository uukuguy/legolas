/**
 * @file   udclient_write_data.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-09-09 14:23:03
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

/* ==================== handle_write_response() ==================== */ 
int udclient_handle_write_response(session_t *session, message_t *response)
{
    assert(response->msg_type == MSG_TYPE_RESPONSE);

    int ret = 0;

    session_rx_off(session);

    if ( response->result != RESULT_SUCCESS ) {
        warning_log("return message is not SUCCESS. result: %d", response->result);
        return -1;
    }

    udclient_t *udcli = UDCLIENT(session);
    if ( udcli->after_write_finished != NULL ){
        udcli->after_write_finished(udcli, response);
    }

    return ret;
}

int do_write_request(session_t *session, char *data, uint32_t data_size);

/* ==================== after_write_request() ==================== */ 
static void after_write_request(uv_write_t *write_req, int status) 
{
    session_t *session = (session_t*)write_req->data;

    udclient_t *udcli = UDCLIENT(session);
    assert(udcli!= NULL);

    zfree(write_req);

    if ( udcli->after_write_object_slice != NULL ){
        msgidx_t *msgidx = zmalloc(sizeof(msgidx_t));
        memset(msgidx, 0, sizeof(msgidx_t));

        msgidx->object_size = udcli->object_size;
        msgidx->slice_idx = udcli->slice_idx;
        msgidx->nslices = udcli->nslices;

        msgidx->key = udcli->key;
        msgidx->keylen = udcli->keylen;
        msgidx->data_size = udcli->data_size;

        udcli->after_write_object_slice(udcli, msgidx);

        zfree(msgidx);
    }
    /* Keep write next block? */
    if ( udcli->total_writed >= udcli->object_size ){
        session_waiting_message(session);
    }
}

/* ==================== do_write_request() ==================== */ 
int do_write_request(session_t *session, char *data, uint32_t data_size)
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

    if ( data_size == 0 ) return 0;

    udclient_t *udcli = UDCLIENT(session);
    udcli->total_writed = 0;

    uint32_t head_size = 0;

    message_t *write_request = alloc_request_message(session->id, MSG_OP_WRITE); 
    head_size += sizeof(message_t);

    const char *key = udcli->key;
    uint32_t keylen = udcli->keylen;
    debug_log("udclient->key=%s, keylen=%d", key, keylen);

    /* -------- key -------- */
    write_request = add_message_arg(write_request, key, keylen > 128 ? 128 : keylen);
    head_size += sizeof(uint32_t) + keylen;

    /* -------- key_md5 -------- */
    md5_value_t md5Value;
    md5(&md5Value, (const uint8_t*)key, keylen);

    write_request = add_message_arg(write_request, &md5Value, sizeof(md5_value_t));
    head_size += sizeof(uint32_t) + sizeof(md5_value_t);

    /* -------- object_size -------- */
    uint32_t object_size = udcli->object_size;
    write_request = add_message_arg(write_request, &object_size, sizeof(object_size));
    head_size += sizeof(uint32_t) + sizeof(object_size);


    /*head_size += sizeof(uint32_t) + sizeof(md5_value_t);*/
    head_size += sizeof(uint32_t) + sizeof(uint32_t); /* CRC32 */

    uint32_t block_size = DEFAULT_SOCKBUF_SIZE - head_size - sizeof(uint32_t); /* data size*/
    block_size = data_size < block_size ? data_size : block_size;
    block_size -= block_size % 512;
    uint32_t writed = block_size;

    /* FIXME */
    char *buf = data;
    /*char *buf = NULL;*/
    /*buf = &client_args->file_data[client_args->file_data_sended];*/
    /*if ( client_args->file_data_sended + block_size > client_args->file_size ){*/
        /*writed = client_args->file_size - client_args->file_data_sended;*/
        /*client_args->file_data_sended += writed;*/
    /*} else {*/
        /*client_args->file_data_sended += block_size;*/
    /*}*/

    session->total_writed += writed;
    udcli->total_writed += writed;
    udcli->data = data;
    udcli->data_size = writed;

    /* -------- CRC32 -------- */
    uint32_t crc = crc32(0, buf, writed);
    write_request = add_message_arg(write_request, &crc, sizeof(crc));

    /* -------- data -------- */
    write_request = add_message_arg(write_request, (uint8_t *)buf, writed);
    session->id++;


    /* -------- ubuf -------- */
    uint32_t msg_size = sizeof(message_t) + write_request->data_length;
    /*trace_log("msg_size:%d head_size:%d writed:%d body_size:%zu", msg_size, head_size, writed,  head_size + sizeof(uint32_t) + writed); */
    assert(msg_size == head_size + sizeof(uint32_t) + writed);

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
            after_write_request);

    zfree(write_request);

    if ( r != 0 ) {
        error_log("uv_write() failed");
        return -1;
    }

    return writed;
}

