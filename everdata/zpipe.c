/**
 * @file   bucket.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-21 15:09:27
 * 
 * @brief  
 * 
 * 
 */
/*
 
   typedef struct slave_t{
        ZPIPE_ACTOR;

   } slave_t;

   void thread_main(zsock_t *pipe, void *user_data){
        slave_t *slave = (slave_t*)user_data;

        ZPIPE_ACTOR_THREAD_BEGIN(pipe);

        ZPIPE_ACTOR_THREAD_END(pipe);
   }

   slave_t *slave_new(){
        slave_t *slave = (slave_t*)malloc(sizeof(slave_t));
        memset(slave, 0, sizeof(slave_t));

        ZPIPE_ACTOR_NEW(slave, thread_main);

        return slave;
   }

   typedef struct master_t{
        ZPIPE;

        slave_t **slaves;
   } master_t;

   master_t *master_new(uint32_t total_actors){
        master_t *master = (master_t*)malloc(sizeof(master_t));
        memset(master, 0, sizeof(master_t));

        ZPIPE_NEW(madster, total_actors);

        for (int i = 0 ; i < total_actors ; i++ ){
            slave_t *slave = slave_new();
            master->slaves[i] = slave;
        }
   }

   void master_free(master_t *master){
        ZPIPE_FREE(master);
   }

   void master_loop(master_t *master){
        ZPIPE_START_ACTORS(master, slaves);
   }
   
*/

#include <czmq.h>
#include "common.h"
#include "logger.h"
#include "everdata.h"
#include "zpipe.h"

#define ZPIPE \
        zpipe_t *zpipe;

#define ZPIPE_NEW(parent, total_actors) \
        parent->zpipe = zpipe_new(total_actors);

#define ZPIPE_FREE(parent) \
    zpipe_free(parent_zpipe); \
    parent->zpipe = NULL;

#define ZPIPE_START_ACTORS(parent, zpipe_actors) \
        zpipe_start_actors(parent->zpipe, (zpipe_actor_t**)parent->zpipe_actors);

#define ZPIPE_ACTOR \
        zpipe_actor_t zpipe_actor;

#define ZPIPE_ACTOR_NEW(actor, actor_thread_main) \
        actor->zpipe_actor->actor = zactor_new(actor_thread_main, actor);

#define ZPIPE_ACTOR_THREAD_BEGIN(pipe) \
        zpipe_actor_thread_begin(pipe);

#define ZPIPE_ACTOR_THREAD_END(pipe) \
        zpipe_actor_thread_end(pipe);

/* ================ zpipe_new() ================ */
zpipe_t *zpipe_new(uint32_t total_actors)
{
    zpipe_t *zpipe = (zpipe_t*)malloc(sizeof(zpipe_t));

    zpipe_init(zpipe, total_actors);

    return zpipe;
}

/* ================ zpipe_init() ================ */
void zpipe_init(zpipe_t *zpipe, uint32_t total_actors)
{
    memset(zpipe, 0, sizeof(zpipe_t));
    zpipe->total_actors = total_actors;
}

/* ================ zpipe_free() ================ */
void zpipe_free(zpipe_t *zpipe)
{
    free(zpipe);
}

/* ================ zpipe_actor_thread_begin() ================ */
void zpipe_actor_thread_begin(zsock_t *pipe)
{
    zsock_signal(pipe, 0);
    message_send_status(pipe, MSG_STATUS_ACTOR_READY);
}

/* ================ zpipe_actor_thread_end() ================ */
void zpipe_actor_thread_end(zsock_t *pipe)
{
    message_send_status(pipe, MSG_STATUS_ACTOR_OVER);
}

/* ================ zpipe_handle_pullin() ================ */
int zpipe_handle_pullin(zloop_t *loop, zsock_t *pipe, void *user_data)
{
    zpipe_actor_t *zpipe_actor = (zpipe_actor_t*)user_data;
    zpipe_t *zpipe = zpipe_actor->zpipe;

    if ( zpipe->total_over_actors >= zpipe->total_actors ){
        zloop_reader_end(loop, pipe);
        return -1;
    }

    zmsg_t *msg = zmsg_recv(pipe);
    if ( msg == NULL ){
        zloop_reader_end(loop, pipe);
        return -1;
    }
    /*zmsg_print(msg);*/

    if ( message_check_status(msg, MSG_STATUS_ACTOR_OVER) == 0 ){
        zpipe->total_over_actors++;
        trace_log("Actor over! (%d/%d)", zpipe->total_over_actors, zpipe->total_actors);
    }

    zmsg_destroy(&msg);

    return 0;
}

/* ================ zpipe_start_actors() ================ */
int zpipe_start_actors(zpipe_t *zpipe, zpipe_actor_t** zpipe_actors)
{
    zloop_t *loop = zloop_new();
    zloop_set_verbose(loop, zpipe->verbose);

    uint32_t total_actors = zpipe->total_actors;
    for ( int i = 0 ; i < total_actors ; i++ ){
        zpipe_actors[i]->zpipe = zpipe;
        zactor_t *actor = zpipe_actors[i]->actor;

        zloop_reader(loop, (zsock_t*)zactor_resolve(actor), zpipe_handle_pullin, zpipe_actors[i]);
    }

    zloop_start(loop);

    zloop_destroy(&loop);

    return 0;
}

