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
#include "filesystem.h"
#include "logger.h"
#include "kvdb.h"
#include "object.h"
#include "zmalloc.h"

vnode_t *vnode_new(char *root_dir, uint32_t id)
{
    vnode_t *vnode = (vnode_t*)zmalloc(sizeof(vnode_t));

    memset(vnode, 0, sizeof(vnode_t));
    vnode->id = id;

    /* Create vnode root dir */
    sprintf(vnode->root_dir, "%s/%04d", root_dir, id);
    if ( mkdir_if_not_exist(vnode->root_dir) != 0 ){
        error_log("Cann't create vnode(%d) dir:%s", id, vnode->root_dir);
        zfree(vnode);
        return NULL;
    }

    /* datazones */
    int i;
    for ( i = 0 ; i < MAX_DATAZONES ; i++ ){
        vnode->datazones[i] = (datazone_t*)zmalloc(sizeof(datazone_t));
        if ( datazone_init(vnode->datazones[i], vnode, i) != 0 ){
            zfree(vnode);
            return NULL;
        }
    }

    /* Index DB */
    char dbpath[NAME_MAX];
    sprintf(dbpath, "%s/manifest.db", vnode->root_dir);
    kvdb_t *kvdb = kvdb_open(dbpath); 
    if ( kvdb == NULL ){
        error_log("kvdb_init() failed. vnode(%d) dir:%s", id, vnode->root_dir);
        zfree(vnode);
        return NULL;
    }
    vnode->kvdb = kvdb;

    vnode->caching_objects = object_queue_new(object_compare_md5_func);

    vnode->received_objects = listCreate();
    vnode->received_object_size = 0;
    vnode->standby_objects = listCreate();
    vnode->standby_object_size = 0;

    return vnode;
}

void vnode_free(vnode_t *vnode){
    if ( vnode->kvdb != NULL ){
        kvdb_close(vnode->kvdb);
        vnode->kvdb = NULL;
    }

    if ( vnode->caching_objects != NULL ){
        object_queue_free(vnode->caching_objects);
        vnode->caching_objects = NULL;
    }

    listRelease(vnode->received_objects);
    vnode->received_object_size = 0;
    listRelease(vnode->standby_objects);
    vnode->standby_object_size = 0;

    /* Index DB */
    if ( vnode->kvdb != NULL ){
        kvdb_close(vnode->kvdb);
        vnode->kvdb = NULL;
    }

    /* datazones */
    int i;
    for ( i = 0 ; i < MAX_DATAZONES ; i++ ){
       if ( vnode->datazones[i] != NULL ){
           datazone_destroy(vnode->datazones[i]);
           zfree(vnode->datazones[i]);
       }
    }

    zfree(vnode);
}

datazone_t *get_datazone_by_object(vnode_t *vnode, object_t *object)
{
    datazone_t *datazone = NULL;

    int d2 = object->key_md5.h2 % MAX_DATAZONES;
    datazone = vnode->datazones[d2];

    return datazone;
}

