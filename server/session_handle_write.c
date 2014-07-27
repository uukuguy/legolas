/**
 * @file   session_handle_write.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-07-07 15:41:53
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
#include <msgpack.h>

/* ==================== parse_write_request() ==================== */ 
int parse_write_request(session_t *session, message_t *request, block_t *block)
{
    /* -------- message args -------- */
    message_arg_t *arg = (message_arg_t*)request->data;
    message_arg_t *argCRC32 = NULL;
    block->id = request->id;
    /*if ( request->id == 0 ) {*/
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

        /* -------- argFileSize -------- */
        message_arg_t *argFileSize = message_next_arg(argMd5);
        block->object_size = *((uint32_t*)argFileSize->data);

        /*trace_log("\n~~~~~~~~ fd(%d) block(%d) ~~~~~~~~\n Try to open new file. \n key=%s object_size=%d\n", session_fd(session), sockbuf->blockid, argKey->data, object_size);*/

        /* -------- argCRC32 -------- */
        argCRC32 = message_next_arg(argFileSize);
    /*} else {*/
        /*argCRC32 = arg;*/
    /*}*/

    message_arg_t *argData = message_next_arg(argCRC32);

    /*debug_log("fd(%d) block(%d) argData: size=%d", session_fd(session), sockbuf->blockid, argData->size);*/
    if ( argData->size > 0 ) {
        /**
         * Check data md5 value.
         */

        if ( check_data_crc32(request->id, argCRC32, argData) != 0 ){
            error_log("fd(%d) request message id(%d) Check buffer crc32 failed.", session_fd(session), request->id);
            return -1;
        }
    } 

    block->data = argData->data;
    block->data_size = argData->size;

    return 0;
}

/* ==================== response_to_write() ==================== */ 
void response_to_write(session_t *session, enum MSG_RESULT result)
{
    session_response(session, result);
}

#ifdef STORAGE_LSM
object_t *write_to_cache(object_queue_t *caching_objects, int blockid, const char *key, md5_value_t key_md5, uint32_t object_size, char *write_buf, uint32_t write_bytes)
{
    object_t obj;
    memcpy(&obj.key_md5, &key_md5, sizeof(md5_value_t));

    object_t *object = object_queue_find(caching_objects, &obj);
    if ( object == NULL ){
        object = object_new(key);
        assert(check_md5(&object->key_md5, &key_md5) == 0 );
        object->object_size = object_size;

        pthread_mutex_lock(&caching_objects->queue_lock);
        object_queue_insert(caching_objects, object);
        pthread_mutex_unlock(&caching_objects->queue_lock);
    }

    slice_t *slice = slice_new(); 
    slice->seq_num = blockid;
    byte_block_write(&slice->byteblock, write_buf, write_bytes);

    listAddNodeTail(object->slices, slice);

    return object;

}
#else

int write_to_file(session_t *session, block_t *block)
{
    char *write_buf = block->data;
    uint32_t write_bytes = block->data_size;

    uint32_t nowrited_bytes = block->object_size - session->total_writed; 
    assert(block->data_size <= nowrited_bytes);

    char *aligned_buf = NULL;

    if ( session->f == NULL ){
        error_log("fd(%d) session->f == NULL", session_fd(session));
        return -1;
    }
    /* is lastest buffer. */
    if ( write_bytes == nowrited_bytes ){
        write_bytes = (block->data_size + 511) & ~(511);
        assert(write_bytes >= block->data_size);
    } else {
        assert(block->data_size % 512 == 0);
        write_bytes = block->data_size;
    }
    assert(write_bytes % 512 == 0);

    int r = posix_memalign((void**)&aligned_buf, 512, DEFAULT_CONN_BUF_SIZE);
    if ( r != 0 ){
        error_log("posix_memalign() return %d. msg: %s", r, strerror(r));
        return -1;
    }

    memset(aligned_buf, 0, write_bytes);

    assert(write_bytes >= block->data_size);
    memcpy(aligned_buf, block->data, block->data_size);
    write_buf = aligned_buf;


    if ( storage_write_file(&session->server->storage, write_buf, write_bytes, session->f) < write_bytes ) {
        zfree(aligned_buf);
        return -1;
    }
    zfree(aligned_buf);

    return 0;
}
#endif /* #ifdef STORAGE_LSM */

#ifdef STORAGE_LSM
int write_to_kvdb(object_t *object, vnode_t *vnode)
{
    object_put_into_kvdb(vnode->kvdb, object);

    object_queue_remove(vnode->caching_objects, object);

    return 0;
}
#endif /* #ifdef STORAGE_LSM */

/* ==================== session_write_block() ==================== */ 
object_t *session_write_block(session_t *session, block_t *block)
{
    UNUSED int r = 0;
    /*char *write_buf = block->data;*/
    uint32_t write_bytes = block->data_size;

    /*if ( block->data_size == 0 ) {*/
        /*return 0;*/
    /*}*/

    /*uint32_t nowrited_bytes = block->object_size - session->total_writed; */
    /*assert(block->data_size <= nowrited_bytes);*/

    /* TODO Attach to vnode's caching_objects */
#ifdef STORAGE_LSM

    char *write_buf = block->data;

    vnode_t *vnode = get_vnode_by_key(SERVER(session), &block->key_md5);
    assert(vnode != NULL);

    object_t *object = write_to_cache(vnode->caching_objects, block->id, block->key, block->key_md5, block->object_size, write_buf, write_bytes);

#else /* #ifdef STORAGE_LSM */

    if ( write_to_file(session, block) != 0 ) {
        error_log("write_to_file() failed.");
        return NULL;
    }

#endif /* #ifdef STORAGE_LSM */

    session->total_writed += write_bytes;

    return object;
}

/* ==================== session_handle_write() ==================== */ 
int session_handle_write(session_t *session, message_t *request)
{
    block_t block;

    /** ----------------------------------------
     *    Parse request
     *  ---------------------------------------- */

    if ( parse_write_request(session, request, &block) != 0 ){
        error_log("parse_write_request() failed. key:%s", block.key);
        return -1;
    }
    /*notice_log("block.object_size:%d", block.object_size);*/

    /** ----------------------------------------
     *    Open target file for write if possible.
     *  ---------------------------------------- */

    if ( block.id == 0 ) {

        assert(session->total_writed == 0);

#ifndef STORAGE_LSM
        /*session->f = storage_open_file(&session->server->storage, block.key, "wb+");*/
        session->f = storage_open_file_by_keymd5(&session->server->storage, &block.key_md5, "wb+");
        if ( session->f == NULL ){
            error_log("storage_open_file() failed. key:%s", block.key);
            return -1;
        }
#endif

    }

    /** ----------------------------------------
     *    Write block!
     *  ---------------------------------------- */

    /*trace_log("fd(%d) block(%d) argData: size=%d", session_fd(session), sockbuf->blockid, argData->size);*/

    object_t *object = session_write_block(session, &block); 
    if (  object == NULL ) {
        error_log("session_write_block() failed.");
        return -1;
    }

    /*session->total_writed += block.data_size;*/

    /** ----------------------------------------
     *    Close storage.
     *  ---------------------------------------- */

    if ( session->total_writed >= block.object_size ){
#ifdef STORAGE_LSM
        vnode_t *vnode = get_vnode_by_key(SERVER(session), &block.key_md5);
        write_to_kvdb(object, vnode);

        /*object_queue_remove(vnode->caching_objects, object);*/
#else
        storage_close_file(&session->server->storage, session->f);
        session->f = NULL;
#endif
        session->total_writed = 0;

        /* -------- response -------- */

        response_to_write(session, RESULT_SUCCESS);
        /*__sync_add_and_fetch(&session->finished_works, 1);*/

        pthread_yield();
    }

    return 0;
}

