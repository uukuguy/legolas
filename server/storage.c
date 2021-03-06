/**
 * @file   storage.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-05-19 19:26:35
 * 
 * @brief  
 * 
 * 
 */

#include "storage.h"
#include "filesystem.h"
#include "md5.h"
#include "logger.h"
#include "zmalloc.h"
#include "common.h"

int storage_init_directories(const char* rootDir)
{
    /*info_log("Storage init directories %s. Waiting ...", rootDir);*/
    /*if ( mkdir_if_not_exist(rootDir) == 0 ){*/
        /*debug_log("%04d.. ", 0);*/
        /*int i, ret;*/
        /*for ( i = 0 ; i < STORAGE_MAX_VNODES ; i++ ){*/
            /*char dirname[1024];*/
            /*sprintf(dirname, "%s/%04d", rootDir, i);*/
            /*ret = mkdir_if_not_exist(dirname);*/
            /*if ( ret != 0 ) {*/
                /*error_log("mkdir failed. dirname:%s", dirname);*/
                /*return ret;*/
            /*}*/

            /*[> ----------------------------- <]*/
            /*int j;*/
            /*for ( j = 0 ; j < STORAGE_MAX_SECTIONS ; j++ ){*/
            /*[>for ( j = 0 ; j < 100 ; j++ ){<]*/
                /*char subdirname[1024];*/
                /*sprintf(subdirname, "%s/%02d", dirname, j);*/
                /*ret = mkdir_if_not_exist(subdirname);*/
                /*if ( ret != 0 ) {*/
                    /*error_log("mkdir failed. dirname:%s", subdirname);*/
                    /*return ret;*/
                /*}*/
            /*}*/
        /*}*/
    /*}*/
    /*info_log("Storage init directories done.");*/
    return 0;
}

int storage_init(storage_t *storage)
{
    const char *storage_dir = storage->storage_dir;
    debug_log("storage_dir:%s\n", storage_dir);

    storage_init_directories(storage_dir);
    return 0;

}

int storage_get_filename_by_key(storage_t *storage, const char *key, char *filename)
{
    md5_value_t md5Key;
    md5(&md5Key, (uint8_t *)key, strlen(key));

    int d0 = md5Key.h0 % STORAGE_MAX_VNODES;

    int d1 = md5Key.h1 % STORAGE_MAX_SECTIONS;
    sprintf(filename, "%s/%04d/%02d/%s.dat", storage->storage_dir, d0, d1, key);
    trace_log("get key(%s) filename(%s)", key, filename);

    /*int d2 = md5Key.h2 % 100;*/
    /*int d3 = md5Key.h3 % 100;*/
    /*sprintf(filename, "%s/%04d/%02d/%02d/%s", storage->storage_dir, d1, d2, d3, key);*/

    return 0;
}

storage_file_t *__storage_open_file(storage_t *storage, const char *filename, const char *fmode)
{
    trace_log("NAME_MAX:%d filename(%zu):%s", NAME_MAX, strlen(filename), filename);

    /* FIXME */
    char dirname[NAME_MAX];
    get_file_parent_full_path(filename, dirname, NAME_MAX);
    trace_log("parent_path(%zu):%s", strlen(dirname), dirname);
    if ( mkdir_if_not_exist(dirname) != 0 ){
        error_log("Can not create dir not exist! dirname:%s, filename:%s", dirname, filename);
        return NULL;
    }

    storage_file_t *storage_file = NULL;
    /*FILE *f = fopen(filename, fmode);*/
    /*if ( f == NULL ){*/

    int f = open(filename, O_CREAT | O_TRUNC | O_WRONLY, 0640);
    /*int f = open(filename, O_CREAT | O_TRUNC | O_WRONLY | O_DIRECT, 0640);*/
    if ( f == -1 ){
        error_log("Open file failed. errno:%d filename:%s", errno, filename);
    } else {
        storage_file = zmalloc(sizeof(storage_file_t));
        memset(storage_file, 0, sizeof(storage_file_t));
        storage_file->f = f;
    }

    return storage_file;
}

storage_file_t *storage_open_file(storage_t *storage, const char *key, const char *fmode)
{
    char filename[NAME_MAX];
    storage_get_filename_by_key(storage, key, filename); 

    return __storage_open_file(storage, filename, fmode);
}

storage_file_t *storage_open_file_by_keymd5(storage_t *storage, md5_value_t *key_md5, const char *fmode)
{
    char filename[NAME_MAX];


    int d0 = key_md5->h0 % STORAGE_MAX_VNODES;
    int d1 = key_md5->h1 % STORAGE_MAX_SECTIONS;

    sprintf(filename, "%s/%04d/%02d/%2.2x-%2.2x-%2.2x-%2.2x.dat", storage->storage_dir, d0, d1, key_md5->h0, key_md5->h1, key_md5->h2, key_md5->h3);
    trace_log("filename:%s", filename);

    return __storage_open_file(storage, filename, fmode);
}

int storage_write_file(storage_t *storage, const char *buf, uint32_t buf_size, storage_file_t *storage_file)
{
    int f = storage_file->f;
    int r = 0;
    r = write(f, buf, buf_size);
    if ( r < buf_size ){
        error_log("Write file failed. errno:%d ", errno);
        return -1;
    }
    return r;
}

int storage_read_file(storage_t *storage, char *buf, uint32_t buf_size, storage_file_t *storage_file)
{
    /*FILE *f = storage_file->f;*/
    /*int r = fread(buf, 1, buf_size, f); */
    int f = storage_file->f;
    int r = read(f, buf, buf_size);
    if ( r < 0 ) {
        /*if ( !feof(f) ){*/
            /*error_log("Read file failed. errno:%d ", ferror(f));*/
            /*return -1;*/
        /*}*/
        error_log("Read file failed. errno:%d ", errno);
    }
    return r;
}

void storage_close_file(storage_t *storage, storage_file_t *storage_file)
{
    if ( storage_file != NULL ) {
        /*FILE *f = storage_file->f;*/
        /*if ( f != NULL )*/
            /*fclose(f);*/
        int f = storage_file->f;
        /*fsync(f);*/
        close(f);
        zfree(storage_file);
    }
}

