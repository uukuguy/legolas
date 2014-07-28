/**
 * @file   byteblock.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-07-07 10:41:31
 * 
 * @brief  
 * 
 * 
 */

#ifndef __BYTE_BLOCK_H__
#define __BYTE_BLOCK_H__

#include "zmalloc.h"
#include <stdint.h>

#define BYTE_BLOCK_INIT_SIZE 8192

/* -------------------- byte_block_t -------------------- */
typedef struct byte_block_t {
    uint32_t capacity;
    uint32_t size;
    char* buf;
} byte_block_t;

byte_block_t *byte_block_new(void);
byte_block_t *byte_block_attach(void *data, uint32_t size);
void byte_block_init(byte_block_t *bb);
void byte_block_release(byte_block_t *bb);
void byte_block_free(void *bb);
int byte_block_write(byte_block_t *bb, const void *data, uint32_t size);

#endif /* __BYTE_BLOCK_H__ */

