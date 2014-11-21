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

typedef struct _zactor_t zactor_t;
typedef struct bucket_t bucket_t;
typedef struct vnode_t vnode_t;
typedef struct work_queue_t work_queue_t;

/* -------- struct container_t -------- */
typedef struct container_t{
    uint32_t id;
    const char *broker_endpoint;
    int64_t heartbeat_at;
    zactor_t *actor;
    char data_dir[NAME_MAX];
    int storage_type;
    int verbose;

    bucket_t **buckets;
    uint32_t total_buckets;
    uint32_t total_over_buckets;

} container_t;

container_t *container_new(int container_id, int total_buckets, int storage_type, const char *broker_endpoint, int verbose);
void container_free(container_t *container);

#endif // __CONTAINER_H__

