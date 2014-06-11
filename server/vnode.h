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

#define MAX_DATAZONES 16

struct datazone_t;

typedef struct vnode_info_t {
    uint32_t id;
    char root_dir[NAME_MAX];

    uint32_t n_datazones;
    datazone_info_t datazone_infos[MAX_DATAZONES];

} vnode_info_t;

int vnode_init(vnode_info_t *vnode_info, char *root_dir, uint32_t id);

#endif /* __VNODE_H__ */

