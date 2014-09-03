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
#include "session_handle.h"
#include "message.h"
#include "object.h"
#include "logger.h"
#include "vnode.h"
#include "kvdb.h"

/* ==================== parse_read_request() ==================== */ 
int parse_read_request(session_t *session, message_t *request, msgidx_t *msgidx)
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

int response_object_slice(session_t *session, kvdb_t *kvdb, object_t *object, uint32_t slice_idx) 
{
    char *buf = NULL;
    uint32_t buf_size = 0;
    int rc = object_get_slice_from_kvdb(kvdb, object->key_md5, slice_idx, (void**)&buf, &buf_size);
    if ( rc == 0 ) {
        if ( slice_idx == object->nslices - 1 ){
            message_t *response = alloc_response_message(0, RESULT_SUCCESS);

            /* ---------- key ---------- */
            response = add_message_arg(response, object->key, object->key != NULL ? strlen(object->key) : 0 );
            /* ---------- slice_idx ---------- */
            response = add_message_arg(response, &slice_idx, sizeof(slice_idx));
            /* ---------- nslices ---------- */
            response = add_message_arg(response, &object->nslices, sizeof(object->nslices));
            /* ---------- object_size ---------- */
            response = add_message_arg(response, &object->object_size, sizeof(object->object_size));
            /* ---------- data ---------- */
            response = add_message_arg(response, buf, buf_size);

            uint32_t msg_size = sizeof(message_t) + response->data_length;
            session_response_data(session, (char *)response, msg_size);
            zfree(response);            
        }

        zfree(buf);

    } else {
        error_log("object_get_slice_from_kvdb() failure.");
        rc = -1;
    }

    return rc;
}

/* ==================== session_handle_read() ==================== */ 
int session_handle_read(session_t *session, message_t *request)
{
    /** ----------------------------------------
     *    Parse request
     *  ---------------------------------------- */

    msgidx_t msgidx;
    msgidx_init(&msgidx);
    if ( parse_read_request(session, request, &msgidx) != 0 ){
        error_log("parse_read_request() failed. key:%s", msgidx.key);
        return -1;
    }

    /** ----------------------------------------
     *    Get object
     *  ---------------------------------------- */

    vnode_t *vnode = get_vnode_by_key(SERVER(session), msgidx.key_md5);
    assert(vnode != NULL);

    object_t *object = object_get_from_kvdb(vnode->kvdb, *msgidx.key_md5);

    /** ----------------------------------------
     *    Response to client 
     *  ---------------------------------------- */
    int ret = 0;
    if ( object != NULL ){
        uint32_t n;
        for ( n = 0 ; n < object->nslices ; n++ ){
            if ( response_object_slice(session, vnode->kvdb, object, n) != 0 ) {
                warning_log("key:%s Slice %d storage failed.", msgidx.key, n);
                ret = -1;
                response_with_key(session, &msgidx, RESULT_ERR_STORAGE_FAILED);
            }
        }
        object_free(object);
        object = NULL;
    } else {
        warning_log("key:%s NOT FOUND.", msgidx.key);
        ret = -1;
        response_with_key(session, &msgidx, RESULT_ERR_NOTFOUND);
    }

    return ret;
}

