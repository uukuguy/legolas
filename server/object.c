/**
 * @file   object.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-06-06 21:40:35
 * 
 * @brief  
 * 
 * 
 */

#include "object.h"
#include "zmalloc.h"
#include "common.h"
#include "md5.h"
#include "kvdb.h"
#include "logger.h"
#include <msgpack.h>

/* -------------------- slice_t -------------------- */
slice_t *slice_new(void)
{
    slice_t *slice = (slice_t*)zmalloc(sizeof(slice_t));
    memset(slice, 0, sizeof(slice_t));
    byte_block_init(&slice->byteblock);
    return slice;
}

void slice_free(void *ptr)
{
    slice_t *slice = (slice_t*)ptr;
    if ( slice != NULL ){
        byte_block_release(&slice->byteblock);
        zfree(slice);
    }
}

/* -------------------- object_t -------------------- */
object_t *object_new(const char *key)
{
    object_t *object = (object_t*)zmalloc(sizeof(object_t));
    memset(object, 0, sizeof(object_t));

    if ( key != NULL ){
        uint32_t keylen = strlen(key);
        object->key = zmalloc(keylen+1);
        memcpy(object->key, key, keylen);
        object->key[keylen] = '\0';

        md5(&object->key_md5, (uint8_t *)object->key, keylen);
    }

    object->slices = listCreate();
    listSetFreeMethod(object->slices, slice_free);
    return object;
}


void object_free(object_t *object)
{
    if ( object->key != NULL ){
        zfree(object->key);
        object->key = NULL;
    }
    if ( object->slices != NULL ) {
        listRelease(object->slices);
        object->slices = NULL;
    }
    zfree(object);
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
    
    msgpack_unpacked_destroy(&result);

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
    
    msgpack_unpacked_destroy(&result);

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
    
    msgpack_unpacked_destroy(&result);

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
    object->nslices = nSlices;

    msgpack_unpacked_destroy(&result);

    return 0;
}

int object_put_into_kvdb(kvdb_t *kvdb, object_t *object)
{
    md5_value_t key_md5 = object->key_md5;

    uint32_t nSlices = listLength(object->slices);

    msgpack_sbuffer *sbuf = msgpack_sbuffer_new();
    msgpack_packer *packer = msgpack_packer_new(sbuf, msgpack_sbuffer_write);

    /* key_md5 */
    msgpack_pack_array(packer, 4);
    msgpack_pack_uint32(packer, object->key_md5.h0);
    msgpack_pack_uint32(packer, object->key_md5.h1);
    msgpack_pack_uint32(packer, object->key_md5.h2);
    msgpack_pack_uint32(packer, object->key_md5.h3);

    /* key */
    uint32_t key_len = strlen(object->key);
    msgpack_pack_raw(packer, key_len);
    msgpack_pack_raw_body(packer, object->key, key_len);

    /* object_size */
    msgpack_pack_uint32(packer, object->object_size);

    /* nSlices */
    msgpack_pack_uint32(packer, nSlices);

    char sz_key_md5[64];
    sprintf(sz_key_md5, "%2.2x%2.2x%2.2x%2.2x", key_md5.h0, key_md5.h1, key_md5.h2, key_md5.h3);
    uint32_t key_md5_len = strlen(sz_key_md5);

    int rc = kvdb_put(kvdb, sz_key_md5, key_md5_len, sbuf->data, sbuf->size);
    msgpack_sbuffer_free(sbuf);
    msgpack_packer_free(packer);

    if ( rc != 0 ) {
        error_log("Storage save key_md5 by kvdb_put() failed. key_md5=%s, key=%s, object_size=%d, slices=%d", sz_key_md5, object->key, object->object_size, nSlices);
        return -1;
    } else {
        trace_log("Storage save key_md5 by kvdb_put() failed. key_md5=%s, key=%s, object_size=%d, slices=%d", sz_key_md5, object->key, object->object_size, nSlices);
    }

    uint32_t seq_num = 0;

    listIter *iter = listGetIterator(object->slices, AL_START_HEAD);
    listNode *node = NULL;
    while ( (node = listNext(iter)) != NULL ){
        slice_t *slice = (slice_t*)node->value;
        char *buf = slice->byteblock.buf;
        uint32_t buf_size = slice->byteblock.size;

        char key[64];
        sprintf(key, "%2.2x%2.2x%2.2x%2.2x-%08d", object->key_md5.h0, object->key_md5.h1, object->key_md5.h2, object->key_md5.h3, seq_num);

        int rc = kvdb_put(kvdb, key, strlen(key), buf, buf_size);
        if ( rc != 0 ) {
            error_log("Storage save by kvdb_put() failed. key=%s seq_num=%d buf_size=%d", key, seq_num, buf_size);
            return -1;
        } else {
            trace_log("Storage save by kvdb_put() OK. key=%s seq_num=%d buf_size=%d", key, seq_num, buf_size);
        }
    }
    listReleaseIterator(iter);

    return 0;
}

        /*uint32_t n;*/
        /*for ( n = 0 ; n < object->nslices ; n++ ) {*/
            /*char key[64];*/
            /*sprintf(key, "%2.2x%2.2x%2.2x%2.2x-%08d", object->key_md5.h0, object->key_md5.h1, object->key_md5.h2, object->key_md5.h3, n);*/
            /*char *buf = NULL;*/
            /*uint32_t buf_size = 0;*/
            /*int rc = kvdb_get(kvdb, sz_key_md5, key_md5_len, (void**)&buf, &buf_size);*/
            /*if ( rc == 0 && buf != NULL && buf_size > 0 ){*/
                /*zfree(buf);*/
            /*} else {*/
                /*error_log("kvdb_get() failed. key:%s, n=%d/%d", key, n, object->nSlices);*/
                /*ret = -1;*/
                /*break;*/
            /*}*/

        /*}*/

int object_get_slice_from_kvdb(kvdb_t *kvdb, md5_value_t key_md5, uint32_t seq_num, void** ppbuf, uint32_t *pbuf_size)
{
    char key[64];
    sprintf(key, "%2.2x%2.2x%2.2x%2.2x-%08d", key_md5.h0, key_md5.h1, key_md5.h2, key_md5.h3, seq_num);
    uint32_t key_len = strlen(key);

    char *buf = NULL;
    uint32_t buf_size = 0;
    int rc = kvdb_get(kvdb, key, key_len, (void**)&buf, &buf_size);
    if ( rc == 0 && buf != NULL && buf_size > 0 ){
        *ppbuf = buf;
        *pbuf_size = buf_size;
    }
    return rc;
}

object_t *object_get_from_kvdb(kvdb_t *kvdb, md5_value_t key_md5)
{
    UNUSED int object_found = 0;
    object_t *object = NULL;

    char sz_key_md5[64];
    sprintf(sz_key_md5, "%2.2x%2.2x%2.2x%2.2x", key_md5.h0, key_md5.h1, key_md5.h2, key_md5.h3);
    uint32_t key_md5_len = strlen(sz_key_md5);

    char *buf = NULL;
    uint32_t buf_size = 0;
    int rc = kvdb_get(kvdb, sz_key_md5, key_md5_len, (void**)&buf, &buf_size);
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

        zfree(buf);
    }

    return object;
}

int object_del_from_kvdb(kvdb_t *kvdb, md5_value_t key_md5)
{
    int ret = 0;
    UNUSED int rc;

    object_t *object = object_get_from_kvdb(kvdb, key_md5);
    if ( object != NULL ){
        char sz_key_md5[64];
        sprintf(sz_key_md5, "%2.2x%2.2x%2.2x%2.2x", key_md5.h0, key_md5.h1, key_md5.h2, key_md5.h3);
        uint32_t key_md5_len = strlen(sz_key_md5);

        rc = kvdb_del(kvdb, sz_key_md5, key_md5_len);
        if ( rc == 0 ){
            uint32_t n;
            for ( n = 0 ; n < object->nslices ; n++ ) {
                char key[64];
                sprintf(key, "%2.2x%2.2x%2.2x%2.2x-%08d", object->key_md5.h0, object->key_md5.h1, object->key_md5.h2, object->key_md5.h3, n);
                uint32_t key_len = strlen(key);
                int rc = kvdb_del(kvdb, key, key_len);
                if ( rc == 0 ){
                } else {
                    error_log("kvdb_del() failed. key:%s, n=%d/%d", key, n, object->nslices);
                    ret = -1;
                    break;
                }
            }
        }
    } else {
        ret = -1;
    }

    return ret;
}

/* -------------------- object_queue_t -------------------- */
int object_compare_key_func(void *first, void *second)
{
    object_t *object_first = (object_t*)first;
    object_t *object_second = (object_t*)second;

    return strcmp(object_first->key, object_second->key);
}

int object_compare_md5_func(void *first, void *second)
{
    if ( first == NULL ) return -1;
    if ( second == NULL ) return 1;
    object_t *object_first = (object_t*)first;
    object_t *object_second = (object_t*)second;

    uint64_t a0 = ((uint64_t)object_first->key_md5.h0 << 32) | object_second->key_md5.h1;
    uint64_t b0 = ((uint64_t)object_second->key_md5.h0 << 32) | object_second->key_md5.h1;
    if ( a0 > b0 ) return 1;
    if ( a0 < b0 ) return -1;

    uint64_t a1 = ((uint64_t)object_first->key_md5.h2 << 32) | object_second->key_md5.h3;
    uint64_t b1 = ((uint64_t)object_first->key_md5.h2 << 32) | object_second->key_md5.h3;
    if ( a1 > b1 ) return 1;
    if ( a1 < b1 ) return -1;

    return 0;
}

object_queue_t *object_queue_new(object_compare_func_t *func)
{
    object_queue_t *oq = (object_queue_t*)zmalloc(sizeof(object_queue_t));
    oq->objects = skiplist_new(16, 0.5, 0, 0, func);
    pthread_mutex_init(&oq->queue_lock, NULL);
    return oq;
}

void object_queue_free(object_queue_t *oq)
{
    if ( oq->objects != NULL ){
        skiplist_free(&oq->objects);
        oq->objects = NULL;
    }

    pthread_mutex_destroy(&oq->queue_lock);

    zfree(oq);
}

void* object_queue_find(object_queue_t *oq, void *query_data)
{
    void *data = skiplist_find_first(oq->objects, query_data, NULL);
    return data;
}

int object_queue_insert(object_queue_t *oq, void *data)
{
    return skiplist_insert(oq->objects, data);
}

void object_queue_remove(object_queue_t *oq, void *query_data)
{
    void *node = NULL;
    void *data = skiplist_find_first(oq->objects, query_data, &node);
    if ( node != NULL ){
        skiplist_delete_node(oq->objects, node);
    }
    if ( data != NULL ){
        object_t * object = (object_t*)data;
        object_free(object);
    }
}

