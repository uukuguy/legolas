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

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include "zpipe.h"

typedef struct _zactor_t zactor_t;
typedef struct container_t container_t;
typedef struct vnode_t vnode_t;
typedef struct channel_t channel_t;

/* -------- struct bucket_t -------- */
typedef struct bucket_t {
    ZPIPE_ACTOR;

    ZPIPE;

    uint32_t id;
    container_t *container;

    uint32_t total_channels;
    const char *broker_endpoint;
    int storage_type;
    int verbose;

    vnode_t *vnode;
    int64_t heartbeat_at;

} bucket_t;

bucket_t *bucket_new(container_t *container, uint32_t bucket_id);
void bucket_free(bucket_t *bucket);
int bucket_handle_message(bucket_t *bucket, zsock_t *sock, zmsg_t *msg);

#ifdef __cplusplus
}
#endif

#endif // __BUCKET_H__

