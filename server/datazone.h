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

struct vnode_t;

typedef struct datazone_info_t{
    uint32_t id;
    char storage_dir[NAME_MAX];
    struct vnode_info_t *vnode_info;

} datazone_info_t;

int datazone_init(datazone_info_t *datazone_info, struct vnode_info_t *vnode_info, int id);

#endif /* __DATAZONE_H__ */

