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

void vnode_write_queue_handle_write(work_queue_t *wq)
{
    void *node_data = NULL;
    while ( (node_data = dequeue_work(wq)) != NULL ){
        vnode_write_queue_entry_t *entry = (vnode_write_queue_entry_t*)node_data;
        session_t *session = entry->session;
        object_t *object = entry->object;
        zfree(entry);
        
        server_write_to_storage(SERVER(session), object);

        /* FIXME 2014-10-10 18:57:20 */
        __sync_add_and_fetch(&session->finished_works, 1);
        /*session_response(session, RESULT_SUCCESS);*/

        /*uint32_t total_committed = __sync_add_and_fetch(&vnode->total_committed, 1);*/
        /*if ( total_committed > 800 ) {*/
        /*__sync_sub_and_fetch(&vnode->total_committed, total_committed);*/
        /*fsync(vnode->logFile);*/
        /*kvdb_flush(vnode->kvdb);*/
        /*}*/

        /* FIXME 2014-10-10 18:56:55 */
        /*session_response(session, RESULT_SUCCESS);*/

        sched_yield();
    }
}

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
    assert(argKey->size > 0);
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

    /*assert(msgidx->data_size > 0);*/


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
            /*assert(1==0);*/
            object_free(object);
            return NULL;
        }

        object_queue_insert(caching_objects, object);
    }

    object_add_slice(object, write_buf, write_bytes);
    /*slice_t *slice = slice_new(); */
    /*slice->seq_num = blockid;*/
    /*byte_block_write(&slice->byteblock, write_buf, write_bytes);*/
    /*listAddNodeTail(object->slices, slice);*/

    session->total_writed += msgidx->data_size;

    return object;

}

/* ==================== server_write_to_storage() ==================== */ 
int server_write_to_storage(server_t *server, object_t *object)
{
    int ret = 0;


    if ( object != NULL ) {
        UNUSED vnode_t *vnode = get_vnode_by_key(server, &object->key_md5);
        assert(vnode != NULL);

        ret = vnode_write_to_storage(vnode, object);

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
            error_log("session_write_to_cache() failed.");
            __sync_add_and_fetch(&session->finished_works, 1);
            session->total_writed = 0;
            ret = -1;
        } else {

            /** ----------------------------------------
             *    Write to storage & Response to client.
             *  ---------------------------------------- */

            if ( session->total_writed >= msgidx.object_size ){

                /* FIXME 2014-10-23 15:49:37 */
                /*vnode_t *vnode = get_vnode_by_key(SERVER(session), &object->key_md5);*/
                /*vnode_enqueue_write_queue(vnode, session, object);*/

                server_write_to_storage(SERVER(session), object);
                session_response(session, RESULT_SUCCESS);

                __sync_add_and_fetch(&session->finished_works, 1);
                __sync_sub_and_fetch(&session->running_tasks, 1);

                /*message_t *response = alloc_response_message(RESULT_SUCCESS);*/
                /*listAddNodeTail(session->responseQueue, response);*/

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

