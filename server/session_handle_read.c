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
#include <msgpack.h>

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
    block->file_size = 0;
    block->data = NULL;
    block->data_size = 0;

    return 0;
}

/* ==================== unpack_object_key_md5() ==================== */ 
int unpack_object_key_md5(msgpack_unpacker *unpacker, object_t *object)
{
    msgpack_unpacked result;
    msgpack_unpacked_init(&result);

    if ( !msgpack_unpacker_next(unpacker, &result) ){
        error_log("unpark key_md5 failed.");
        return -1;
    } 

    msgpack_object *mobj = &result.data;                    
    if ( mobj->type != MSGPACK_OBJECT_ARRAY ){
        return -1;
    }

    msgpack_object_array *mobj_array = &mobj->via.array;
    msgpack_object *obj0 = &mobj_array->ptr[0];
    object->key_md5.h0 = obj0->via.u64;
    msgpack_object *obj1 = &mobj_array->ptr[1];
    object->key_md5.h1 = obj1->via.u64;
    msgpack_object *obj2 = &mobj_array->ptr[2];
    object->key_md5.h2 = obj2->via.u64;
    msgpack_object *obj3 = &mobj_array->ptr[3];
    object->key_md5.h3 = obj3->via.u64;
    trace_log("key_md5: %2.2x%2.2x%2.2x%2.2x", object->key_md5.h0, object->key_md5.h1, object->key_md5.h2, object->key_md5.h3); 
    
    return 0;
}

/* ==================== unpack_object_key() ==================== */ 
int unpack_object_key(msgpack_unpacker *unpacker, object_t *object)
{
    msgpack_unpacked result;
    msgpack_unpacked_init(&result);

    if ( !msgpack_unpacker_next(unpacker, &result) ){
        error_log("unpark key failed.");
        return -1;
    } 

    msgpack_object *mobj = &result.data;                    
    if ( mobj->type != MSGPACK_OBJECT_RAW ){
        return -1;
    }

    msgpack_object_raw *mobj_raw = &mobj->via.raw;
    trace_log("key: %s ", mobj_raw->ptr);

    object->key = zmalloc(mobj_raw->size + 1);
    memcpy(object->key, mobj_raw->ptr, mobj_raw->size);
    object->key[mobj_raw->size] = '\0';
    
    return 0;
}

/* ==================== unpack_object_object_size() ==================== */ 
int unpack_object_object_size(msgpack_unpacker *unpacker, object_t *object)
{
    msgpack_unpacked result;
    msgpack_unpacked_init(&result);

    if ( !msgpack_unpacker_next(unpacker, &result) ){
        error_log("unpark key failed.");
        return -1;
    } 

    msgpack_object *mobj = &result.data;                    
    if ( mobj->type != MSGPACK_OBJECT_POSITIVE_INTEGER ){
        return -1;
    }

    uint32_t object_size = (uint32_t)mobj->via.u64;
    trace_log("object_size: %d ", object_size);
    object->object_size = object_size;
    
    return 0;
}

/* ==================== unpack_object_nslices() ==================== */ 
int unpack_object_nslices(msgpack_unpacker *unpacker, object_t *object)
{
    msgpack_unpacked result;
    msgpack_unpacked_init(&result);

    if ( !msgpack_unpacker_next(unpacker, &result) ){
        error_log("unpark key failed.");
        return -1;
    } 

    msgpack_object *mobj = &result.data;                    
    if ( mobj->type != MSGPACK_OBJECT_POSITIVE_INTEGER ){
        return -1;
    }

    uint32_t nSlices = (uint32_t)mobj->via.u64;
    trace_log("nSlices: %d ", nSlices);

    return 0;
}


static void after_response_to_read(uv_write_t *write_rsp, int status) 
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
        response = add_response_arg(response, object->key, strlen(object->key));
        response = add_response_arg(response, &object->object_size, sizeof(object->object_size));
        msg_size = sizeof(msg_response_t) + response->data_length;
    } else {
        response = alloc_response(0, RESULT_ERR_NOTFOUND);
        warning_log("key:%s NOT FOUND.", block->key);
        response = add_response_arg(response, block->key, strlen(block->key));
        msg_size = sizeof(msg_response_t) + response->data_length;
    }

    session_send_data(session, (char *)response, msg_size, after_response_to_read);

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
        int object_found = 0;
        object_t obj;
        memcpy(&obj.key_md5, &block.key_md5, sizeof(md5_value_t));
        object_t *object = object_queue_find(vnode->caching_objects, &obj);
        if ( object != NULL ) {
            object_found = 1;
        }
        
        if ( !object_found ) {
            trace_log("key:%s NOT FOUND in cache.", block.key);
            char key_md5[1024];
            sprintf(key_md5, "%2.2x%2.2x%2.2x%2.2x", obj.key_md5.h0, obj.key_md5.h1, obj.key_md5.h2, obj.key_md5.h3);
            uint32_t key_md5_len = strlen(key_md5);

            char *buf = NULL;
            uint32_t buf_size = 0;
            int rc = kvdb_get(vnode->kvdb, key_md5, key_md5_len, (void**)&buf, &buf_size);
            if ( rc == 0 && buf != NULL && buf_size > 0 ){
                msgpack_unpacker unpacker;
                msgpack_unpacker_init(&unpacker, buf_size);

                msgpack_unpacker_reserve_buffer(&unpacker, buf_size);
                memcpy(msgpack_unpacker_buffer(&unpacker), buf, buf_size);
                msgpack_unpacker_buffer_consumed(&unpacker, buf_size);

                object = object_new(NULL);


                if ( unpack_object_key_md5(&unpacker, object) == 0 ){
                    if ( unpack_object_key(&unpacker, object) == 0 ){
                        if ( unpack_object_object_size(&unpacker, object) == 0 ){
                            if ( unpack_object_nslices(&unpacker, object) == 0 ){
                                object_found = 1;
                            }
                        }
                    }
                }

                msgpack_unpacker_destroy(&unpacker);
                /* FIXME */
                /*zfree(buf);*/

                if ( !object_found ) {
                    object_free(object);
                    object = NULL;
                }
            }
        }

        response_to_read(session, &block, object);
    }

#endif

    return 0;
}

