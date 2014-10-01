/**
 * @file   vnode.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-06-06 21:52:29
 * 
 * @brief  
 * 
 * 
 */

#ifndef __VNODE_H__
#define __VNODE_H__

#include "common.h"
#include "datazone.h"
#include "object.h"

#define MAX_DATAZONES 16

typedef struct kvdb_t kvdb_t;
typedef struct work_queue_t work_queue_t;
typedef struct session_t session_t;

typedef struct datazone_t datazone_t;

typedef enum eVnodeStorageType {
    STORAGE_NONE = 0,
    STORAGE_KVDB,
    STORAGE_LOGFILE
} eVnodeStorageType;

typedef struct vnode_t {
    uint32_t id;
    char root_dir[NAME_MAX];
    enum eVnodeStorageType storage_type;

    uint32_t n_datazones;
    datazone_t *datazones[MAX_DATAZONES];

    kvdb_t *kvdb;
    int logFile;
    uint32_t total_committed;

    object_queue_t *caching_objects;

    list *received_objects; 
    uint32_t received_object_size;
    list *standby_objects; /* objects waiting for write */
    uint32_t standby_object_size;

    work_queue_t *kvdb_queue;

} vnode_t;

typedef struct vnode_kvdb_queue_entry_t{
    session_t *session;
    object_t *object;
} vnode_kvdb_queue_entry_t;

vnode_t *vnode_new(char *root_dir, uint32_t id, enum eVnodeStorageType storage_type);
void vnode_free(vnode_t *vnode);

datazone_t *get_datazone_by_object(vnode_t *vnode, object_t *object);

int vnode_write_to_storage(vnode_t *vnode, object_t *object);
object_t *vnode_read_from_storage(vnode_t *vnode, md5_value_t key_md5);
int vnode_get_slice_from_storage(vnode_t *vnode, md5_value_t key_md5, uint32_t slice_idx, void** ppbuf, uint32_t *pbuf_size);
#endif /* __VNODE_H__ */

