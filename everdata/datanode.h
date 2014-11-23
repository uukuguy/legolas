/**
 * @file   datanode.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-23 20:46:15
 * 
 * @brief  
 * 
 * 
 */

#ifndef __DATANODE_H__
#define __DATANODE_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include "zpipe.h"

typedef struct datanode_t{
    ZPIPE;

    uint32_t total_containers;
    uint32_t total_buckets;
    uint32_t total_channels;
    int storage_type;
    const char *broker_endpoint;
    int verbose;

} datanode_t;

datanode_t *datanode_new(uint32_t total_containers, uint32_t total_buckets, uint32_t total_channels, int storage_type, const char *broker_endpoint, int verbose);
void datanode_free(datanode_t *datanode);
void datanode_loop(datanode_t *datanode);

#ifdef __cplusplus
}
#endif

#endif // __DATANODE_H__


