/**
 * @file   object.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-06-06 21:38:27
 * 
 * @brief  
 * 
 * 
 */

#ifndef __OBJECT_H__
#define __OBJECT_H__

#include "adlist.h"
#include "md5.h"
#include "skiplist.h"
#include <pthread.h>

/* -------------------- slice_t -------------------- */
typedef struct slice_t {
    uint32_t seq_num;
    char *buf;
    uint32_t buf_size;
} slice_t;

/* -------------------- object_t -------------------- */
typedef struct object_t
{
    char *key;
    md5_value_t key_md5;
    uint32_t object_size;

    list *slices;
    uint32_t unfinished_size;
} object_t;

object_t *object_new(const char *key);
void object_free(object_t *object);

/* -------------------- object_queue_t -------------------- */
typedef struct object_queue_t {
    skiplist *objects;

	pthread_mutex_t queue_lock;

} object_queue_t;

typedef int (object_compare_func_t)(void *, void *);

int object_compare_key_func(void *first, void *second);
int object_compare_md5_func(void *first, void *second);

object_queue_t *object_queue_new(object_compare_func_t *func);
void object_queue_free(object_queue_t *oq);
void* object_queue_find(object_queue_t *oq, void *query_data);
int object_queue_insert(object_queue_t *oq, void *data);

#endif /* __OBJECT_H__ */

