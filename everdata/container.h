/**
 * @file   container.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-21 15:13:11
 * 
 * @brief  
 * 
 * 
 */

#ifndef __CONTAINER_H__
#define __CONTAINER_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "zpipe.h"

typedef struct _zactor_t zactor_t;
typedef struct datanode_t datanode_t;
typedef struct bucket_t bucket_t;
typedef struct vnode_t vnode_t;
typedef struct work_queue_t work_queue_t;

/* -------- struct container_t -------- */
typedef struct container_t{
    ZPIPE_ACTOR;

    ZPIPE;

    uint32_t id;
    datanode_t *datanode;

    uint32_t total_buckets;
    uint32_t total_channels;
    int storage_type;
    const char *broker_endpoint;
    int verbose;

    int64_t heartbeat_at;
    char data_dir[NAME_MAX];

} container_t;

container_t *container_new(datanode_t *datanode, uint32_t container_id);
void container_free(container_t *container);

#ifdef __cplusplus
}
#endif

#endif // __CONTAINER_H__

