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
#include "session_handle.h"
#include "object.h"
#include "logger.h"
#include "kvdb.h"
#include "vnode.h"

/* ==================== parse_delete_request() ==================== */ 
int parse_delete_request(session_t *session, message_t *request, msgidx_t *msgidx)
{
    /* -------- message args -------- */
    message_arg_t *arg = (message_arg_t*)request->data;
    msgidx->message = request;

    /* -------- argKey -------- */
    message_arg_t *argKey = arg;
    if ( argKey->size > 0 ) {
        uint32_t keylen = argKey->size < NAME_MAX - 1 ? argKey->size : NAME_MAX - 1;
        msgidx->key = argKey->data;
        msgidx->keylen = keylen;
    } else {
        return -1;
    }

    /* -------- argMd5 -------- */
    message_arg_t *argMd5 = message_next_arg(argKey);
    msgidx->key_md5 = (md5_value_t*)argMd5->data;

    return 0;
}

/* ==================== server_handle_delete() ==================== */ 
int server_handle_delete(session_t *session, message_t *request)
{
    /** ----------------------------------------
     *    Parse request
     *  ---------------------------------------- */

    msgidx_t msgidx;
    msgidx_init(&msgidx);
    if ( parse_delete_request(session, request, &msgidx) != 0 ){
        error_log("parse_delete_request() failed. key:%s", msgidx.key);
        return -1;
    }

    /** ----------------------------------------
     *    Delete object
     *  ---------------------------------------- */

    int object_deleted = 0;

    vnode_t *vnode = get_vnode_by_key(SERVER(session), msgidx.key_md5);
    if ( vnode != NULL ) {
        int rc = vnode_delete_from_storage(vnode, *msgidx.key_md5);
        if ( rc == 0 ){
            object_deleted = 1;
        }
    }

    /** ----------------------------------------
     *    Response to client 
     *  ---------------------------------------- */
    int result = object_deleted != 0 ? RESULT_SUCCESS : RESULT_ERR_NOTFOUND;

    response_with_key(session, &msgidx, result);

    return 0;
}

