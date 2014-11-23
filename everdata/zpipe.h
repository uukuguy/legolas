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
    void **actors;

    void *user_data;
} zpipe_t;

typedef struct zpipe_actor_t{
    zactor_t *actor;
    zpipe_t *zpipe;
} zpipe_actor_t;

zpipe_t *zpipe_new(uint32_t total_actors);
void zpipe_init(zpipe_t *zpipe, uint32_t total_actors);
void zpipe_free(zpipe_t *zpipe);
int zpipe_loop(zpipe_t *zpipe);

void zpipe_actor_thread_begin(zsock_t *pipe);
void zpipe_actor_thread_end(zsock_t *pipe);

#define ZPIPE \
        zpipe_t *zpipe; \

#define ZPIPE_NEW_BEGIN(master, total_slaves) \
        master->zpipe = zpipe_new(total_slaves); \
        master->zpipe->actors = (void**)malloc(sizeof(void*) * total_slaves); \
        for (int i = 0 ; i < total_slaves ; i++ ){ \

#define ZPIPE_NEW_END(master, slave) \
        master->zpipe->actors[i] = slave; \
    }

#define ZPIPE_FREE(master, slave_free) \
    for ( uint32_t i = 0 ; i < master->zpipe->total_actors ; i++ ){ \
        slave_free(master->zpipe->actors[i]); \
        master->zpipe->actors[i] = NULL; \
    } \
    free(master->zpipe->actors);\
    master->zpipe->actors = NULL; \
    zpipe_free(master->zpipe); \
    master->zpipe = NULL;

#define ZPIPE_LOOP(master) \
        zpipe_loop(master->zpipe);

#define ZPIPE_ACTOR \
        zpipe_actor_t zpipe_actor;

#define ZPIPE_ACTOR_NEW(slave, slave_thread_main) \
        slave->zpipe_actor.actor = zactor_new(slave_thread_main, slave);

#define ZPIPE_ACTOR_FREE(slave) \
    zactor_destroy(&slave->zpipe_actor.actor); \
    slave->zpipe_actor.actor = NULL;


#define ZPIPE_ACTOR_THREAD_BEGIN(pipe) \
        zpipe_actor_thread_begin(pipe);

#define ZPIPE_ACTOR_THREAD_END(pipe) \
        zpipe_actor_thread_end(pipe);

#endif // __ZPIPE_H__

