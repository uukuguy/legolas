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
 
void md5(struct md5_value_t *md5_value, uint8_t *initial_msg, size_t initial_len);
int check_md5(struct md5_value_t *md5Keep, struct md5_value_t *md5Data);

#endif /* __MD5_H__ */

