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
#include "protocol.h"
#include "object.h"
#include "logger.h"
#include "vnode.h"
#include "kvdb.h"

/* ==================== parse_read_request() ==================== */ 
int parse_read_request(session_t *session, msg_request_t *request, block_t *block)
{
    /* -------- message args -------- */
    msg_arg_t *arg = (msg_arg_t*)request->data;
    block->id = request->id;

    /* -------- argKey -------- */
    msg_arg_t *argKey = arg;
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
    msg_arg_t *argMd5 = next_arg(argKey);
    memcpy(&block->key_md5, argMd5->data, sizeof(md5_value_t));

    block->id = 0;
    block->object_size = 0;
    block->data = NULL;
    block->data_size = 0;

    return 0;
}

UNUSED static void after_response_to_read(uv_write_t *write_rsp, int status) 
{
    /*session_t *session = (session_t*)write_rsp->data;*/
    /*uv_async_send(&session->async_handle);*/
    zfree(write_rsp);
}

static void response_to_read(session_t *session, block_t *block, object_t *object)
{
    msg_response_t *response;
    uint32_t msg_size = 0;

    if ( object != NULL ){
        response = alloc_response(0, RESULT_SUCCESS);
        response = add_response_arg(response, object->key, object->key != NULL ? strlen(object->key) : 0 );
        response = add_response_arg(response, &object->object_size, sizeof(object->object_size));
        msg_size = sizeof(msg_response_t) + response->data_length;
    } else {
        response = alloc_response(0, RESULT_ERR_NOTFOUND);
        warning_log("key:%s NOT FOUND.", block->key);
        response = add_response_arg(response, block->key, strlen(block->key));
        msg_size = sizeof(msg_response_t) + response->data_length;
    }

    session_response(session, (char *)response, msg_size);
    /*session_send_data(session, (char *)response, msg_size, after_response_to_read);*/

    zfree(response);
}

/* ==================== session_handle_read() ==================== */ 
int session_handle_read(session_t *session, msg_request_t *request)
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

    vnode_t *vnode = get_vnode_by_key(session->server, &block.key_md5);

    if ( vnode != NULL ) {

        object_t *object = object_get_from_kvdb(vnode->kvdb, block.key_md5);

        response_to_read(session, &block, object);

        if ( object != NULL ){
            object_free(object);
            object = NULL;
        }
    }

#endif

    return 0;
}

