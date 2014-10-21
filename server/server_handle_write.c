/**
 * @file   server_handle_write.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-07-07 15:41:53
 * 
 * @brief  
 * 
 * 
 */

#include "server.h"
#include "session.h"
#include "server_handle.h"
#include "message.h"
#include "object.h"
#include "logger.h"
#include "vnode.h"
#include "md5.h"
/*#include "react_utils.h"*/
#include <msgpack.h>

/* ==================== parse_write_request() ==================== */ 
int parse_write_request(session_t *session, message_t *request, msgidx_t *msgidx)
{
    if ( request->data_length <= 0 ) {
        warning_log("request->data_length <= 0!");
        return -1;
    }

    msgidx->message = request;

    /* -------- message args -------- */
    message_arg_t *arg = (message_arg_t*)request->data;
    message_arg_t *argCRC32 = NULL;

    /* -------- argKey -------- */
    message_arg_t *argKey = arg;
    if ( argKey->size > 0 ) {
        uint32_t keylen = argKey->size < NAME_MAX - 1 ? argKey->size : NAME_MAX - 1;
        msgidx->key = argKey->data;
        msgidx->keylen = keylen;
    } else {
        /*uuid_t uuid;*/
        /*uuid_generate(uuid);*/
        /*uuid_unparse(uuid, block->key);*/
        return -1;
    }
    /* -------- argMd5 -------- */
    message_arg_t *argMd5 = message_next_arg(argKey);
    msgidx->key_md5 = (md5_value_t*)argMd5->data;

    /* -------- argFileSize -------- */
    message_arg_t *argFileSize = message_next_arg(argMd5);
    msgidx->object_size = *((uint32_t*)argFileSize->data);

    /* -------- argCRC32 -------- */
    argCRC32 = message_next_arg(argFileSize);

    /* -------- argData -------- */
    message_arg_t *argData = message_next_arg(argCRC32);

    /*if ( argData->size > 0 ) {*/
        /*if ( check_data_crc32(request->id, argCRC32, argData) != 0 ){*/
            /*error_log("fd(%d) request message id(%d) Check buffer crc32 failed.", session_fd(session), request->id);*/
            /*return -1;*/
        /*}*/
    /*} */

    msgidx->data = argData->data;
    msgidx->data_size = argData->size;


    return 0;
}

object_t *server_write_to_cache(session_t *session, msgidx_t *msgidx)
{

    vnode_t *vnode = get_vnode_by_key(SERVER(session), msgidx->key_md5);
    assert(vnode != NULL);
    object_queue_t *caching_objects = vnode->caching_objects;

    int blockid = msgidx->message->id;
    uint32_t object_size = msgidx->object_size; 
    const char *write_buf = msgidx->data;
    uint32_t write_bytes = msgidx->data_size;;

    object_t obj;
    memset(&obj, 0, sizeof(object_t));
    memcpy(&obj.key_md5, msgidx->key_md5, sizeof(md5_value_t));
    object_t *object = object_queue_find(caching_objects, &obj);
    /*object_t *object = object_queue_find(caching_objects, &msgidx->key_md5);*/

    if ( object == NULL ){
        object = object_new(msgidx->key, msgidx->keylen);
        object->object_size = object_size;

        if ( check_md5(&object->key_md5, msgidx->key_md5) != 0 ){
            error_log("Check md5 error. session(%d), block_id:%d, key:%s object->key:%s", session->id, blockid, msgidx->key, object->key);
            object_free(object);
            return NULL;
        }

        object_queue_insert(caching_objects, object);
    }

    slice_t *slice = slice_new(); 
    slice->seq_num = blockid;
    byte_block_write(&slice->byteblock, write_buf, write_bytes);

    listAddNodeTail(object->slices, slice);

    session->total_writed += msgidx->data_size;

    return object;

}

/* ==================== server_write_to_storage() ==================== */ 
/* called by vnode_write_queue_handle_write() in vnode.c */
int server_write_to_storage(session_t *session, object_t *object)
{
    int ret = 0;


    if ( object != NULL ) {
        UNUSED vnode_t *vnode = get_vnode_by_key(SERVER(session), &object->key_md5);
        assert(vnode != NULL);

        ret = vnode_write_to_storage(vnode, object);

        object_queue_remove(vnode->caching_objects, object);
    } else {
        ret = -1;
    }

    return ret;
}

/* ==================== server_handle_write() ==================== */ 
int server_handle_write(session_t *session, message_t *request)
{
    int ret = 0;
    msgidx_t msgidx;
    msgidx_init(&msgidx);

    /** ----------------------------------------
     *    Parse request
     *  ---------------------------------------- */

    if ( parse_write_request(session, request, &msgidx) != 0 ){
        error_log("parse_write_request() failed. key:%s", msgidx.key);
        ret = -1;
    } else {

        if ( session->total_writed == 0 ){
            __sync_add_and_fetch(&session->running_tasks, 1);
        }
        /** ----------------------------------------
         *    Write cache
         *  ---------------------------------------- */

        object_t *object = server_write_to_cache(session, &msgidx);
        if (  object == NULL ) {
            error_log("session_write_block() failed.");
            ret = -1;
        } else {

            /** ----------------------------------------
             *    Write to storage & Response to client.
             *  ---------------------------------------- */

            if ( session->total_writed >= msgidx.object_size ){

                vnode_t *vnode = get_vnode_by_key(SERVER(session), &object->key_md5);
                vnode_enqueue_write_queue(vnode, session, object);
                session->total_writed = 0;

                /* FIXME */
                /*vnode_write_queue_entry_t *entry = (vnode_write_queue_entry_t*)zmalloc(sizeof(vnode_write_queue_entry_t));*/
                /*memset(entry, 0, sizeof(vnode_write_queue_entry_t));*/
                /*entry->session = session;*/
                /*entry->object = object;*/

                /*enqueue_work(vnode->write_queue, entry);*/


                /*pthread_yield();*/
                /*sched_yield();*/
            } 
        }
    }

    return ret;
}

