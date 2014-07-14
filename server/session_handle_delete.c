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
#include "protocol.h"
#include "object.h"
#include "logger.h"
#include "kvdb.h"
#include "vnode.h"

/* ==================== parse_delete_request() ==================== */ 
int parse_delete_request(session_t *session, msg_request_t *request, block_t *block)
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
        return -1;
    }

    /* -------- argMd5 -------- */
    msg_arg_t *argMd5 = next_arg(argKey);
    memcpy(&block->key_md5, argMd5->data, sizeof(md5_value_t));

    block->id = 0;
    block->file_size = 0;
    block->data = NULL;
    block->data_size = 0;

    return 0;
}

static void after_response_to_delete(uv_write_t *write_rsp, int status) 
{
    zfree(write_rsp);
}

void response_to_delete(session_t *session, enum MSG_RESULT result)
{
    msg_response_t *response = alloc_response(0, result);

    uint32_t msg_size = sizeof(msg_response_t) + response->data_length;

    session_send_data(session, (char *)response, msg_size, after_response_to_delete);

    zfree(response);
}

/* ==================== session_handle_delete() ==================== */ 
int session_handle_delete(session_t *session, msg_request_t *request)
{
    /** ----------------------------------------
     *    Parse request
     *  ---------------------------------------- */

    block_t block;
    if ( parse_delete_request(session, request, &block) != 0 ){
        error_log("parse_delete_request() failed. key:%s", block.key);
        return -1;
    }

    vnode_t *vnode = get_vnode_by_key(session->server, &block.key_md5);

    int object_deleted = 0;
    if ( vnode != NULL ) {

        char key_md5[1024];
        sprintf(key_md5, "%2.2x%2.2x%2.2x%2.2x", block.key_md5.h0, block.key_md5.h1, block.key_md5.h2, block.key_md5.h3);
        uint32_t key_md5_len = strlen(key_md5);

        int rc = kvdb_del(vnode->kvdb, key_md5, key_md5_len);
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
