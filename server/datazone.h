/**
 * @file  datazone.h 
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-06-06 21:53:03
 * 
 * @brief  
 * 
 * 
 */

#ifndef __DATAZONE_H__
#define __DATAZONE_H__

#include "common.h"

typedef struct vnode_t vnode_t;

typedef struct datazone_t{
    uint32_t id;
    char storage_dir[NAME_MAX];
    vnode_t *vnode;

} datazone_t;

int datazone_init(datazone_t *datazone, struct vnode_t *vnode, int id);
void datazone_destroy(datazone_t *datazone);

#endif /* __DATAZONE_H__ */

