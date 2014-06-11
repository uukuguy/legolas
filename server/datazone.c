/**
 * @file  datazone.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-06-06 21:54:39
 * 
 * @brief  
 * 
 * 
 */

#include "datazone.h"
#include "vnode.h"
#include "logger.h"

int datazone_init(datazone_info_t *datazone_info, struct vnode_info_t *vnode_info, int id)
{
    memset(datazone_info, 0, sizeof(datazone_info_t));
    datazone_info->vnode_info = vnode_info;
    datazone_info->id = id;
    sprintf(datazone_info->storage_dir, "%s/%02d", vnode_info->root_dir, id);
    if ( mkdir_if_not_exist(vnode_info->root_dir) == 0 ){
        return 0;
    } else {
        error_log("Cann't create datazone(%d) dir:%s", id, datazone_info->storage_dir);
        return -1;
    }
}

