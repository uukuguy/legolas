/**
 * @file   datanode.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-23 20:45:10
 * 
 * @brief  
 * 
 * 
 */

#include "common.h"
#include "logger.h"
#include "everdata.h"

#include "container.h"
#include "datanode.h"

datanode_t *datanode_new(uint32_t total_containers, uint32_t total_buckets, int storage_type, const char *endpoint, int verbose)
{
    datanode_t *datanode = (datanode_t*)malloc(sizeof(datanode_t));
    memset(datanode, 0, sizeof(datanode_t));

    datanode->total_containers = total_containers;
    datanode->total_buckets = total_buckets;
    datanode->storage_type = storage_type;
    datanode->endpoint = endpoint;
    datanode->verbose = verbose;

    return datanode;
}

/* ================ datanode_free() ================ */
void datanode_free(datanode_t *datanode)
{
    ZPIPE_FREE(datanode, container_free);

    free(datanode);
}

/* ================ datanode_loop() ================ */
void datanode_loop(datanode_t *datanode)
{

    ZPIPE_NEW_BEGIN(datanode, datanode->total_containers);

    container_t *container = container_new(i, 
            datanode->total_buckets, 
            datanode->storage_type, 
            datanode->endpoint, 
            datanode->verbose);

    ZPIPE_NEW_END(datanode, container);

    ZPIPE_LOOP(datanode);
}

