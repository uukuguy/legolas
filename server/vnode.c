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
#include "object.h"
#include "zmalloc.h"
#include "work.h"
#include "server.h"
#include "session.h"

#define VNODE_WRITE_QUEUE_INTERVAL 1 /* ms */

vnode_t *vnode_new(const char *root_dir, uint32_t id, enum eVnodeStorageType storage_type, vnode_write_queue_handle_write_cb vnode_write_queue_handle_write)
{
    vnode_t *vnode = (vnode_t*)zmalloc(sizeof(vnode_t));

    memset(vnode, 0, sizeof(vnode_t));
    vnode->id = id;
    vnode->storage_type = storage_type;

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
    if ( vnode->storage_type >= STORAGE_KVDB ){
        char dbpath[NAME_MAX];
        sprintf(dbpath, "%s/manifest.db", vnode->root_dir);

        kvdb_t *kvdb = NULL;

        if ( vnode->storage_type == STORAGE_KVDB ){
            kvdb = kvdb_open("lmdb", dbpath);
        } else if ( vnode->storage_type == STORAGE_KVDB_EBLOB ){
            kvdb = kvdb_open("eblob", dbpath);
        } else if ( vnode->storage_type == STORAGE_KVDB_LMDB ){
            kvdb = kvdb_open("lmdb", dbpath);
        } else if ( vnode->storage_type == STORAGE_KVDB_LEVELDB ){
            kvdb = kvdb_open("leveldb", dbpath);
        } else if ( vnode->storage_type == STORAGE_KVDB_ROCKSDB ){
            kvdb = kvdb_open("rocksdb", dbpath);
        } else if ( vnode->storage_type == STORAGE_KVDB_LSM ){
            kvdb = kvdb_open("lsm", dbpath);
        }

        if ( kvdb == NULL ){
            error_log("kvdb_init() failed. vnode(%d) dir:%s", id, vnode->root_dir);
            zfree(vnode);
            return NULL;
        }
        vnode->kvdb = kvdb;
    }

    vnode->caching_objects = object_queue_new(object_compare_md5_func);

    vnode->received_objects = listCreate();
    vnode->received_object_size = 0;
    vnode->standby_objects = listCreate();
    vnode->standby_object_size = 0;

    vnode->write_queue = init_work_queue(vnode_write_queue_handle_write, VNODE_WRITE_QUEUE_INTERVAL);

    return vnode;
}

void vnode_free(vnode_t *vnode){

    work_queue_t *wq = vnode->write_queue;
    if ( wq != NULL ) {
        exit_work_queue(wq);
        zfree(wq);
        vnode->write_queue = NULL;
    }

    if ( vnode->logFile != 0 ){
        close(vnode->logFile);
        vnode->logFile = 0;
    }

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

/* ==================== vnode_write_to_file() ==================== */ 
int vnode_write_to_file(vnode_t *vnode, object_t *object)
{
    int logFile = vnode->logFile;
    if ( logFile == 0 ) {
        char log_filename[NAME_MAX];
        sprintf(log_filename, "%s/vnode.log", vnode->root_dir);
        logFile = open(log_filename, O_APPEND | O_CREAT | O_WRONLY, 0640);
        vnode->logFile = logFile;
    }

    object_put_into_file(logFile, object);

    return 0;
}

/* ==================== vnode_write_to_kvdb() ==================== */ 
int vnode_write_to_kvdb(vnode_t *vnode, object_t *object)
{
    /* FIXME */
    object_put_into_kvdb(vnode->kvdb, object);

    return 0;
}

/* ==================== vnode_write_to_storage() ==================== */ 
int vnode_write_to_storage(vnode_t *vnode, object_t *object)
{
    int ret = 0;

    if ( vnode->storage_type >= STORAGE_KVDB ){
        ret = vnode_write_to_kvdb(vnode, object);
    } else if ( vnode->storage_type == STORAGE_LOGFILE ){
        ret = vnode_write_to_file(vnode, object);
    }

    if ( ret == 0 ) {
        object_queue_remove(vnode->caching_objects, object);
    }

    return ret;
}

object_t *vnode_read_from_storage(vnode_t *vnode, md5_value_t key_md5)
{
    object_t *object = NULL;
    
    if ( vnode->storage_type >= STORAGE_KVDB ){
        object = object_get_from_kvdb(vnode->kvdb, key_md5);
    } else if ( vnode->storage_type == STORAGE_LOGFILE ){
    } 

    return object;
}

int vnode_delete_from_storage(vnode_t *vnode, md5_value_t key_md5)
{
    int rc = 0;

    if ( vnode->storage_type >= STORAGE_KVDB ){
        rc = object_del_from_kvdb(vnode->kvdb, key_md5);
    } else if ( vnode->storage_type == STORAGE_LOGFILE ){
    } 

    return rc;
}

int vnode_get_slice_from_storage(vnode_t *vnode, md5_value_t key_md5, uint32_t slice_idx, void** ppbuf, uint32_t *pbuf_size)
{
    int ret = -1;

    if ( vnode->storage_type >= STORAGE_KVDB ){
        ret = object_get_slice_from_kvdb(vnode->kvdb, key_md5, slice_idx, ppbuf, pbuf_size);
    } else if ( vnode->storage_type == STORAGE_LOGFILE ){
    }

    return ret;
}

void vnode_enqueue_write_queue(vnode_t *vnode, session_t *session, object_t *object)
{

    vnode_write_queue_entry_t *entry = (vnode_write_queue_entry_t*)zmalloc(sizeof(vnode_write_queue_entry_t));
    memset(entry, 0, sizeof(vnode_write_queue_entry_t));
    entry->session = session;
    entry->object = object;

    enqueue_work(vnode->write_queue, entry);
}

