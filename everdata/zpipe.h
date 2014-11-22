/**
 * @file   zpipe.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-22 00:41:29
 * 
 * @brief  
 * 
 * 
 */

#ifndef __ZPIPE_H__
#define __ZPIPE_H__

#include <stdint.h>
typedef struct _zactor_t zactor_t;

typedef struct zpipe_t {
    uint32_t total_actors;
    uint32_t total_over_actors;
    int verbose;

    void *user_data;
} zpipe_t;

typedef struct zpipe_actor_t{
    zactor_t *actor;
    zpipe_t *zpipe;
} zpipe_actor_t;

zpipe_t *zpipe_new(uint32_t total_actors);
void zpipe_init(zpipe_t *zpipe, uint32_t total_actors);
void zpipe_free(zpipe_t *zpipe);
int zpipe_start_actors(zpipe_t *zpipe, zpipe_actor_t** zpipe_actors);

void zpipe_actor_thread_begin(zsock_t *pipe);
void zpipe_actor_thread_end(zsock_t *pipe);

#define ZPIPE \
        zpipe_t *zpipe;

#define ZPIPE_NEW(master, total_actors) \
        master->zpipe = zpipe_new(total_actors);

#define ZPIPE_FREE(master, slaves) \
    for ( uint32_t i = 0 ; i < master->zpipe->total_actors ; i++ ){ \
        client_free(master->slaves[i]); \
        master->slaves[i] = NULL; \
    } \
    zpipe_free(maste->zpipe); \
    master->zpipe = NULL;

#define ZPIPE_START_ACTORS(master, zpipe_actors) \
        zpipe_start_actors(master->zpipe, (zpipe_actor_t**)master->zpipe_actors);

#define ZPIPE_ACTOR \
        zpipe_actor_t zpipe_actor;

#define ZPIPE_ACTOR_NEW(slave, slave_thread_main) \
        slave->zpipe_actor->actor = zactor_new(slave_thread_main, slave);

#define ZPIPE_ACTOR_THREAD_BEGIN(pipe) \
        zpipe_actor_thread_begin(pipe);

#define ZPIPE_ACTOR_THREAD_END(pipe) \
        zpipe_actor_thread_end(pipe);

#endif // __ZPIPE_H__

