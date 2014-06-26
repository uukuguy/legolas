/**
 * @file   storage.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-05-19 19:27:37
 * 
 * @brief  
 * 
 * 
 */

#ifndef __STORAGE_H__
#define __STORAGE_H__

#include "common.h"

#define STORAGE_MAX_VNODES 64
#define STORAGE_MAX_SECTIONS 16

typedef struct md5_value_t md5_value_t;

typedef struct storage_file_t {
    //FILE *f;
    int f;
} storage_file_t;

typedef struct storage_t {

  char storage_dir[NAME_MAX];

} storage_t;

int storage_init(storage_t *storage);

storage_file_t *storage_open_file(storage_t *storage, const char *key, const char *fmode);
storage_file_t *storage_open_file_by_keymd5(storage_t *storage, md5_value_t *key_md5, const char *fmode);

int storage_write_file(storage_t *storage, const char *buf, uint32_t buf_size, storage_file_t *storage_file);

int storage_read_file(storage_t *storage, char *buf, uint32_t buf_size, storage_file_t *storage_file);

void storage_close_file(storage_t *storage, storage_file_t *storage_file);

#endif /* __STORAGE_H__ */

