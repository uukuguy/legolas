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

slicedb_t *slicedb_new(uint32_t id, kvdb_t *kvdb, uint64_t max_dbsize)
{
    slicedb_t *slicedb = (slicedb_t*)zmalloc(sizeof(slicedb_t));

    slicedb_init(slicedb, id, kvdb, max_dbsize);

    return slicedb;
}

void slicedb_init(slicedb_t *slicedb, uint32_t id, kvdb_t *kvdb, uint64_t max_dbsize)
{
    memset(slicedb, 0, sizeof(slicedb_t));
    slicedb->id = id;
    slicedb->kvdb = kvdb,
    slicedb->max_dbsize = max_dbsize;
}

void slicedb_free(slicedb_t *slicedb)
{
    if ( slicedb->kvdb != NULL ){
        kvdb_close(slicedb->kvdb);
        slicedb->kvdb = NULL;
    }
    zfree(slicedb);
}

void slicedb_free(slicedb_t *slicedb);
kvenv_t *vnode_create_kvenv(vnode_t *vnode, const char *dbpath)
{
    kvenv_t *kvenv = NULL;
    uint64_t max_dbsize = vnode->max_dbsize;
    uint32_t max_dbs = 4;

    if ( vnode->storage_type == STORAGE_KVDB ){
        kvenv = kvenv_new("lmdb", dbpath, max_dbsize, max_dbs);
    } else if ( vnode->storage_type == STORAGE_KVDB_LMDB ){
        kvenv = kvenv_new("lmdb", dbpath, max_dbsize, max_dbs);
    } else if ( vnode->storage_type == STORAGE_KVDB_LSM ){
        kvenv = kvenv_new("lsm", dbpath, max_dbsize, max_dbs);
    } else if ( vnode->storage_type == STORAGE_KVDB_ROCKSDB ){
        kvenv = kvenv_new("rocksdb", dbpath, max_dbsize, max_dbs);
    } else if ( vnode->storage_type == STORAGE_KVDB_LEVELDB ){
        kvenv = kvenv_new("leveldb", dbpath, max_dbsize, max_dbs);
    } else if ( vnode->storage_type == STORAGE_KVDB_EBLOB ){
        kvenv = kvenv_new("eblob", dbpath, max_dbsize, max_dbs);
    }

    if ( kvenv == NULL ){
        error_log("kvenv_new() failed. vnode(%d) dbpath:%s", vnode->id, dbpath);
    }

    return kvenv;
}

kvdb_t *vnode_open_kvdb(vnode_t *vnode, const char *dbname)
{
    char dbpath[NAME_MAX];
    sprintf(dbpath, "%s/%s", vnode->root_dir, dbname);

    kvdb_t *kvdb = NULL;

    kvenv_t *kvenv = vnode_create_kvenv(vnode, dbpath);
    if ( kvenv != NULL ){
        kvdb = kvdb_open(kvenv, "slices");
        if ( kvdb == NULL ){
            error_log("kvdb_init() failed. vnode(%d) dbname:%s dbpath:%s", vnode->id, dbname, dbpath);
        }
    }

    return kvdb;
}

slicedb_t *vnode_open_slicedb(vnode_t *vnode, uint32_t db_id)
{
    slicedb_t *slicedb = NULL;

    char dbname[NAME_MAX];
    sprintf(dbname, "slice-%03d", db_id);
    kvdb_t *kvdb = vnode_open_kvdb(vnode, dbname);
    if ( kvdb != NULL ){
        slicedb = slicedb_new(db_id, kvdb, vnode->max_dbsize);
        vnode->slicedbs[db_id] = slicedb;
    } else {
        error_log("SliceDB create failed. dbname:%s", dbname);
    }

    return slicedb;
}

vnode_t *vnode_new(const char *root_dir, uint32_t id, enum eVnodeStorageType storage_type, vnode_write_queue_handle_write_cb vnode_write_queue_handle_write)
{
    vnode_t *vnode = (vnode_t*)zmalloc(sizeof(vnode_t));

    memset(vnode, 0, sizeof(vnode_t));
    vnode->id = id;
    vnode->storage_type = storage_type;
    /*vnode->max_dbsize = 1024L * 1024L * 1024L * 4L;*/
    vnode->max_dbsize = 1024L * 1024L * 500L;

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

    /* Slices DB */
    if ( vnode->storage_type >= STORAGE_KVDB ){

        // Create Metadata DB.
        const char *metadata_dbname = "metadata";
        kvdb_t *kvdb_metadata = vnode_open_kvdb(vnode, metadata_dbname);
        if ( kvdb_metadata == NULL ){
            error_log("MetadataDB create failed. dbname:%s", metadata_dbname);
            zfree(vnode);
            return NULL;
        }
        vnode->kvdb_metadata = kvdb_metadata;

        uint32_t active_slicedb_id = 0;
        if ( kvdb_get_uint32(kvdb_metadata, "active_slicedb_id", &active_slicedb_id) != 0 ){
            active_slicedb_id = 0;
        }
        notice_log("vnode active_slicedb_id:%d", active_slicedb_id);

        // Create Slice DB.
        for ( int db_id = 0 ; db_id <= active_slicedb_id ; db_id++ ){
            slicedb_t *slicedb = vnode_open_slicedb(vnode, db_id);
            if ( slicedb != NULL ){
            } else {
            /*char dbname[NAME_MAX];*/
            /*sprintf(dbname, "slice-%03d", db_id);*/
            /*kvdb_t *kvdb = vnode_open_kvdb(vnode, dbname);*/
            /*if ( kvdb != NULL ){*/
                /*vnode->slicedbs[db_id] = slicedb_new(db_id, kvdb, vnode->max_dbsize);*/
            /*} else {*/
                /*error_log("SliceDB create failed. dbname:%s", dbname);*/
                for ( int n = 0 ; n < db_id ; n++ ){
                    slicedb_free(vnode->slicedbs[n]);
                    vnode->slicedbs[n] = NULL;
                }
                zfree(vnode);
                return NULL;
            }
        }
        vnode->active_slicedb = vnode->slicedbs[active_slicedb_id];
        /*vnode->kvdb = vnode->active_slicedb->kvdb;*/

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

    if ( vnode->active_slicedb != NULL ){
        uint32_t active_slicedb_id = vnode->active_slicedb->id;
        for ( int db_id = 0 ; db_id < active_slicedb_id ; db_id++ ){
            if ( vnode->slicedbs[db_id] != NULL ){
                slicedb_free(vnode->slicedbs[db_id]);
                vnode->slicedbs[db_id] = NULL;
            }
        }
    }

    if ( vnode->kvdb_metadata != NULL ){
        kvdb_close(vnode->kvdb_metadata);
        vnode->kvdb_metadata = NULL;
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
    /*if ( vnode->kvdb != NULL ){*/
        /*kvdb_close(vnode->kvdb);*/
        /*vnode->kvdb = NULL;*/
    /*}*/

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

int vnode_put_metadata(vnode_t *vnode, const char *key, const char *data, uint32_t data_size)
{
    assert(vnode->kvdb_metadata != NULL);

    return kvdb_put(vnode->kvdb_metadata, key, strlen(key), (void*)data, data_size);
}

int vnode_get_metadata(vnode_t *vnode, const char *key, char **data, uint32_t *data_size)
{
    return kvdb_get(vnode->kvdb_metadata, key, strlen(key), (void**)data, data_size);
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
    /*object_put_into_kvdb(vnode->kvdb, object);*/
    object_put_into_kvdb(vnode->active_slicedb->kvdb, object);

    return 0;
}

typedef struct slice_metadata_t{
    uint32_t version;
    uint32_t slicedb_id;
} slice_metadata_t;

uint32_t vnode_get_slicedb_id(vnode_t *vnode, md5_value_t *key_md5)
{
    uint32_t slicedb_id = vnode->active_slicedb->id;

    slice_metadata_t *slice_metadata = NULL;
    uint32_t value_len = 0;
    if ( kvdb_get(vnode->kvdb_metadata, (const char*)key_md5, sizeof(md5_value_t), (void**)&slice_metadata, &value_len) == 0 ){
        if ( slice_metadata != NULL && value_len == sizeof(slice_metadata_t) ){
            slicedb_id = slice_metadata->slicedb_id;
        }
    }

    return slicedb_id;
}

/* ==================== vnode_write_to_storage() ==================== */ 
int vnode_write_to_storage(vnode_t *vnode, object_t *object)
{
    int ret = 0;


    if ( vnode->storage_type >= STORAGE_KVDB ){
        md5_value_t *key_md5 = &object->key_md5;
        uint32_t slicedb_id = vnode_get_slicedb_id(vnode, key_md5);

        int try_to_write_full_db = 0;
        if ( kvenv_get_dbsize(vnode->active_slicedb->kvdb->kvenv) > 0.9 * vnode->active_slicedb->max_dbsize ){
            uint32_t active_slicedb_id = vnode->active_slicedb->id;
            if ( active_slicedb_id == slicedb_id ){
                try_to_write_full_db = 1;
            }
            slicedb_t *slicedb = vnode_open_slicedb(vnode, active_slicedb_id + 1);
            vnode->active_slicedb = slicedb;

            if ( kvdb_put_uint32(vnode->kvdb_metadata, "active_slicedb_id", active_slicedb_id) != 0 ){
                error_log("Save active_slicedb_id failed. vnode->id:%d active_slicedb_id:%d", vnode->id, active_slicedb_id + 1);
            }
        }

        slicedb_t *active_slicedb = vnode->active_slicedb;

        if ( slicedb_id < vnode->active_slicedb->id ){
            if ( try_to_write_full_db ){
                ret = object_del_from_kvdb(vnode->slicedbs[slicedb_id]->kvdb, *key_md5);
            } else {
                active_slicedb = vnode->slicedbs[slicedb_id];
            }
        }

        slice_metadata_t slice_metadata;
        memset(&slice_metadata, 0, sizeof(slice_metadata_t));
        slice_metadata.version = 0;
        slice_metadata.slicedb_id = active_slicedb->id;
        ret = kvdb_put(vnode->kvdb_metadata, (const char *)key_md5, sizeof(md5_value_t), (void*)&slice_metadata, sizeof(slice_metadata_t)); 
        if ( ret == 0 ){
            /*ret = vnode_write_to_kvdb(vnode, object);*/
            ret = object_put_into_kvdb(active_slicedb->kvdb, object);
        } else {
            error_log("Write metadata failed. vnode->id:%d object->key:%s", vnode->id, object->key);
        }
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
        uint32_t slicedb_id = vnode_get_slicedb_id(vnode, &key_md5);
        object = object_get_from_kvdb(vnode->slicedbs[slicedb_id]->kvdb, key_md5);
    } else if ( vnode->storage_type == STORAGE_LOGFILE ){
    } 

    return object;
}

int vnode_delete_from_storage(vnode_t *vnode, md5_value_t key_md5)
{
    int rc = -1;

    if ( vnode->storage_type >= STORAGE_KVDB ){
        uint32_t slicedb_id = vnode_get_slicedb_id(vnode, &key_md5);
        rc = object_del_from_kvdb(vnode->slicedbs[slicedb_id]->kvdb, key_md5);
    } else if ( vnode->storage_type == STORAGE_LOGFILE ){
    } 

    return rc;
}

int vnode_get_slice_from_storage(vnode_t *vnode, md5_value_t key_md5, uint32_t slice_idx, void** ppbuf, uint32_t *pbuf_size)
{
    int ret = -1;

    if ( vnode->storage_type >= STORAGE_KVDB ){
        /*ret = object_get_slice_from_kvdb(vnode->kvdb, key_md5, slice_idx, ppbuf, pbuf_size);*/
        ret = object_get_slice_from_kvdb(vnode->active_slicedb->kvdb, key_md5, slice_idx, ppbuf, pbuf_size);
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

