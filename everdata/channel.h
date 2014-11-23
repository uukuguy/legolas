/**
 * @file   channel.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-21 23:40:09
 * 
 * @brief  
 * 
 * 
 */

#ifndef __CHANNEL_H__
#define __CHANNEL_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

typedef struct _zactor_t zactor_t;
typedef struct _zsock_t zsock_t;
typedef struct bucket_t bucket_t;

/* -------- struct channel_t -------- */
typedef struct channel_t {
    ZPIPE_ACTOR;

    bucket_t *bucket;

    uint32_t id;
    const char *broker_endpoint;
    int64_t heartbeat_at;

    zsock_t *broker_sock;

} channel_t;

channel_t *channel_new(bucket_t *bucket, uint32_t channel_id);
void channel_free(channel_t *channel);

#ifdef __cplusplus
}
#endif

#endif // __CHANNEL_H__


