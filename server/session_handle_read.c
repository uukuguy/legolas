/**
 * @file   session_handle_read.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-07-07 15:37:57
 * 
 * @brief  
 * 
 * 
 */

#include "server.h"
#include "session.h"
#include "message.h"
#include "object.h"
#include "logger.h"
#include "vnode.h"
#include "kvdb.h"

/* ==================== parse_read_request() ==================== */ 
int parse_read_request(session_t *session, message_t *message, block_t *block)
{
    /* -------- message args -------- */
    message_arg_t *arg = (message_arg_t*)message->data;
    block->id = message->id;

    /* -------- argKey -------- */
    message_arg_t *argKey = arg;
    if ( argKey->size > 0 ) {
        uint32_t keylen = argKey->size < NAME_MAX - 1 ? argKey->size : NAME_MAX - 1;
        memcpy(block->key, argKey->data, keylen);
        block->key[keylen] = '\0';
    } else {
        uuid_t uuid;
        uuid_generate(uuid);
        uuid_unparse(uuid, block->key);
    }

    /* -------- argMd5 -------- */
    message_arg_t *argMd5 = message_next_arg(argKey);
    memcpy(&block->key_md5, argMd5->data, sizeof(md5_value_t));

    block->id = 0;
    block->object_size = 0;
    block->data = NULL;
    block->data_size = 0;

    return 0;
}

typedef struct response_data_t {
    session_t *session;
    kvdb_t *kvdb;
    object_t *object;
    uint32_t seq_num;
} response_data_t;

void response_object_slices(response_data_t *response_data);

static void after_response_to_read(uv_write_t *write_rsp, int status) 
{
    /*response_data_t *response_data = (response_data_t*)write_rsp->data;*/

    /*object_t *object = response_data->object;*/
    /*uint32_t seq_num = response_data->seq_num;*/
    /*if ( seq_num < object->nslices ){*/
        /*response_object_slices(response_data);*/
    /*}*/

    zfree(write_rsp);
}

void response_object_slices(response_data_t *response_data) 
{
    session_t *session = response_data->session;
    kvdb_t *kvdb = response_data->kvdb;
    object_t *object = response_data->object;
    uint32_t seq_num = response_data->seq_num;

    char *buf = NULL;
    uint32_t buf_size = 0;
    int rc = object_get_slice_from_kvdb(kvdb, object->key_md5, seq_num, (void**)&buf, &buf_size);
    if ( rc == 0 ) {
        if ( seq_num == object->nslices - 1 ){
            message_t *response = alloc_response_message(0, RESULT_SUCCESS);
            response = add_message_arg(response, object->key, object->key != NULL ? strlen(object->key) : 0 );
            response = add_message_arg(response, &object->object_size, sizeof(object->object_size));
            /*response = add_message_arg(response, &seq_num, sizeof(seq_num));*/
            /*response = add_message_arg(response, &object->nslices, sizeof(object->nslices));*/
            /*response = add_message_arg(response, buf, buf_size);*/
            uint32_t msg_size = sizeof(message_t) + response->data_length;

            session_send_data(session, (char *)response, msg_size, (void*)response_data, after_response_to_read);
        }

        zfree(buf);

    } else {
        error_log("object_get_slice_from_kvdb() failure.");
    }
}

static void response_to_read(session_t *session, block_t *block, object_t *object, kvdb_t *kvdb)
{

    if ( object != NULL ){
        /* FIXME */
        response_data_t response_data;
        response_data.session = session;
        response_data.kvdb = kvdb;
        response_data.object = object;

        uint32_t n;
        for ( n = 0 ; n < object->nslices ; n++ ){
            response_data.seq_num = n;
            response_object_slices(&response_data);
        }


        /*message_t *response = alloc_response_message(0, RESULT_SUCCESS);*/
        /*response = add_message_arg(response, object->key, object->key != NULL ? strlen(object->key) : 0 );*/
        /*response = add_message_arg(response, &object->object_size, sizeof(object->object_size));*/
        /*uint32_t msg_size = sizeof(message_t) + response->data_length;*/

        /*[>notice_log("FOUND key=%s object_size=%d", object->key, object->object_size);<]*/
        /*session_send_data(session, (char *)response, msg_size, (void*)response, after_response_to_read);*/

        /*zfree(response);*/
    } else {
        message_t *response = alloc_response_message(0, RESULT_ERR_NOTFOUND);
        warning_log("key:%s NOT FOUND.", block->key);
        response = add_message_arg(response, block->key, strlen(block->key));
        uint32_t msg_size = sizeof(message_t) + response->data_length;

        session_response_data(session, (char *)response, msg_size);

        zfree(response);
    }
}

/* ==================== session_handle_read() ==================== */ 
int session_handle_read(session_t *session, message_t *request)
{
    /** ----------------------------------------
     *    Parse request
     *  ---------------------------------------- */

    block_t block;
    if ( parse_read_request(session, request, &block) != 0 ){
        error_log("parse_read_request() failed. key:%s", block.key);
        return -1;
    }

#ifdef STORAGE_LSM

    vnode_t *vnode = get_vnode_by_key(SERVER(session), &block.key_md5);

    if ( vnode != NULL ) {

        object_t *object = object_get_from_kvdb(vnode->kvdb, block.key_md5);

        response_to_read(session, &block, object, vnode->kvdb);

        if ( object != NULL ){
            object_free(object);
            object = NULL;
        }
    }

#endif

    return 0;
}

