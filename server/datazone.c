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
#include "filesystem.h"
#include "logger.h"

int datazone_init(datazone_t *datazone, struct vnode_t *vnode, int id)
{
    memset(datazone, 0, sizeof(datazone_t));
    datazone->vnode = vnode;
    datazone->id = id;
    sprintf(datazone->storage_dir, "%s/%02d", vnode->root_dir, id);
    if ( mkdir_if_not_exist(datazone->storage_dir) == 0 ){
        return 0;
    } else {
        error_log("Cann't create datazone(%d) dir:%s", id, datazone->storage_dir);
        return -1;
    }
}

void datazone_destroy(datazone_t *datazone)
{
}

