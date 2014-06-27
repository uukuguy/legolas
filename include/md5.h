/**
 * @file   md5.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-05-19 21:14:36
 * 
 * @brief  
 * 
 * 
 */
#ifndef __MD5_H__
#define __MD5_H__

#include "common.h"

typedef struct md5_value_t{
    uint32_t h0;
    uint32_t h1;
    uint32_t h2; 
    uint32_t h3;
} md5_value_t;
 
void md5(md5_value_t *md5_value, const uint8_t *initial_msg, uint32_t initial_len);
int check_md5(md5_value_t *md5Keep, struct md5_value_t *md5Data);

void *md5_init(void);
int md5_upadte(void *md5_ctx, const uint8_t *buf, uint32_t buf_size);
int md5_final(void *md5_ctx, md5_value_t *md5_value);

void md5_value_to_string(md5_value_t *md5_value, char *buf);

#endif /* __MD5_H__ */

