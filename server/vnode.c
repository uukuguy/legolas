/**
 * @file   vnode.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-06-06 21:52:00
 * 
 * @brief  
 * 
 * 
 */

#include "vnode.h"
#include "datazone.h"
#include "filesystem.h"
#include "logger.h"
#include "kvdb.h"

int vnode_init(vnode_info_t *vnode_info, char *root_dir, uint32_t id)
{
    memset(vnode_info, 0, sizeof(vnode_info_t));
    vnode_info->id = id;
    sprintf(vnode_info->root_dir, "%s/%04d", root_dir, id);
    if ( mkdir_if_not_exist(vnode_info->root_dir) == 0 ){
        int i;
        for ( i = 0 ; i < MAX_DATAZONES ; i++ ){
            struct datazone_info_t *datazone_info = &vnode_info->datazone_infos[i];
            if ( datazone_init(datazone_info, vnode_info, i) != 0 ){
                return -1;
            }
        }


        /* Init Index DB */
        char dbpath[NAME_MAX];
        sprintf(dbpath, "%s/manifest.db", vnode_info->root_dir);
        struct kvdb_t *kvdb = kvdb_open(dbpath); 
        if ( kvdb != NULL ){
            vnode_info->kvdb = kvdb;
            return 0;
        } else {
            error_log("kvdb_init() failed. vnode(%d) dir:%s", id, vnode_info->root_dir);
            return -1;
        }
    } else {
        error_log("Cann't create vnode(%d) dir:%s", id, vnode_info->root_dir);
        return -1;
    }
}

void vnode_destroy(vnode_info_t *vnode_info){
    if ( vnode_info->kvdb != NULL ){
        kvdb_close(vnode_info->kvdb);
        vnode_info->kvdb = NULL;
    }
}

