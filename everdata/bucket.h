/**
 * @file   bucket.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-21 15:14:41
 * 
 * @brief  
 * 
 * 
 */

#ifndef __BUCKET_H__
#define __BUCKET_H__

#include <stdint.h>

typedef struct _zactor_t zactor_t;
typedef struct container_t container_t;
typedef struct vnode_t vnode_t;
typedef struct work_queue_t work_queue_t;

typedef struct bucket_t bucket_t;

/* -------- struct channel_t -------- */
typedef struct channel_t {
    bucket_t *bucket;

    uint32_t id;
    const char *broker_endpoint;
    int64_t heartbeat_at;
    zactor_t *actor;

    zsock_t *broker_sock;

} channel_t;

/* -------- struct bucket_t -------- */
typedef struct bucket_t {
    uint32_t id;
    const char *broker_endpoint;
    int64_t heartbeat_at;
    zactor_t *actor;
    vnode_t *vnode;
    container_t *container;

    channel_t **channels;
    uint32_t total_channels;
    uint32_t total_over_channels;


    work_queue_t *write_queue;
    work_queue_t *read_queue;
    work_queue_t *delete_queue;

} bucket_t;

bucket_t *bucket_new(int bucket_id, container_t *container, int storage_type, const char *broker_endpoint);
void bucket_free(bucket_t *bucket);

#endif // __BUCKET_H__

