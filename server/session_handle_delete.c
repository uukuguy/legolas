/**
 * @file   session_handle_delete.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-07-07 15:43:01
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
#include "kvdb.h"
#include "vnode.h"

/* ==================== parse_delete_request() ==================== */ 
int parse_delete_request(session_t *session, message_t *request, block_t *block)
{
    /* -------- message args -------- */
    message_arg_t *arg = (message_arg_t*)request->data;
    block->id = request->id;

    /* -------- argKey -------- */
    message_arg_t *argKey = arg;
    if ( argKey->size > 0 ) {
        uint32_t keylen = argKey->size < NAME_MAX - 1 ? argKey->size : NAME_MAX - 1;
        memcpy(block->key, argKey->data, keylen);
        block->key[keylen] = '\0';
    } else {
        return -1;
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

UNUSED static void after_response_to_delete(uv_write_t *write_rsp, int status) 
{
    zfree(write_rsp);
}

void response_to_delete(session_t *session, enum MSG_RESULT result)
{
    message_t *response = alloc_response_message(0, result);

    uint32_t msg_size = sizeof(message_t) + response->data_length;

    session_response_data(session, (char *)response, msg_size);

    zfree(response);
}

/* ==================== session_handle_delete() ==================== */ 
int session_handle_delete(session_t *session, message_t *request)
{
    /** ----------------------------------------
     *    Parse request
     *  ---------------------------------------- */

    block_t block;
    if ( parse_delete_request(session, request, &block) != 0 ){
        error_log("parse_delete_request() failed. key:%s", block.key);
        return -1;
    }

    vnode_t *vnode = get_vnode_by_key(SERVER(session), &block.key_md5);

    int object_deleted = 0;
    if ( vnode != NULL ) {

        int rc = object_del_from_kvdb(vnode->kvdb, block.key_md5);
        if ( rc == 0 ){
            object_deleted = 1;
        }

    }

    if ( object_deleted ){
        response_to_delete(session, RESULT_SUCCESS);
    } else {
        response_to_delete(session, RESULT_ERR_NOTFOUND);
    }

    return 0;
}
