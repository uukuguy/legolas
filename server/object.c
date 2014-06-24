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

object_t *object_new(const char *key)
{
    object_t *object = (object_t*)zmalloc(sizeof(object_t));
    memset(object, 0, sizeof(object_t));

    uint32_t keylen = strlen(key);
    object->key = zmalloc(keylen+1);
    memcpy(object->key, key, keylen);
    object->key[keylen] = '\0';

    md5(&object->key_md5, (uint8_t *)object->key, keylen);

    object->slices = listCreate();
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

int object_compare_key_func(void *first, void *second)
{
    object_t *object_first = (object_t*)first;
    object_t *object_second = (object_t*)second;

    return strcmp(object_first->key, object_second->key);
}

int object_compare_md5_func(void *first, void *second)
{
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

