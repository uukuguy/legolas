/**
 * @file   byteblock.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-07-07 11:09:59
 * 
 * @brief  
 * 
 * 
 */

#include "byteblock.h"
#include "common.h"
#include "logger.h"
#include "zmalloc.h"

byte_block_t *byte_block_new(void)
{
    byte_block_t *bb = (byte_block_t*)zmalloc(sizeof(byte_block_t));
    memset(bb, 0, sizeof(byte_block_t)); 
    return bb;
}

byte_block_t *byte_block_attach(void *data, uint32_t size)
{
    byte_block_t *bb = byte_block_new();
    bb->capacity = size;
    bb->size = size;
    bb->buf = data;
    return bb;
}

void byte_block_init(byte_block_t *bb)
{
    memset(bb, 0, sizeof(byte_block_t));
}

void byte_block_release(byte_block_t *bb)
{
    if ( bb->buf != NULL ){
        zfree(bb->buf);
    }
    byte_block_init(bb);
}

void byte_block_free(void *ptr)
{
    if ( ptr != NULL ){
        byte_block_release((byte_block_t*)ptr);
        zfree(ptr);
    }
}

int byte_block_write(byte_block_t *bb, void *data, uint32_t size)
{
    if ( bb->capacity - bb->size < size ){
        uint32_t total_size = (bb->capacity) ? bb->capacity * 2 : BYTE_BLOCK_INIT_SIZE;
        while ( total_size < bb->size + size ) total_size *= 2;

        char *buf = zrealloc(bb->buf, total_size);
        if ( buf == NULL ){
            error_log("zrealloc() failure!");
            return -1;
        }

        bb->buf = buf;
        bb->capacity = total_size;
    }
    memcpy(bb->buf + bb->size, data, size);
    bb->size += size;

    return 0;
}



