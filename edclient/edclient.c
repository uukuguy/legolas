/**
 * @file   edclient.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-08 03:48:12
 * 
 * @brief  
 * 
 * 
 */

#include <czmq.h>
#include "common.h"

#define NUM_ACTORS 100
#define MAX_COUNT 1000

#define ACTOR_READY "ACTOR READY"
#define ACTOR_OVER "ACTOR OVER"

extern const char *edbroker_frontend_endpoint;

typedef struct client_t {
    int id;
    const char *file_data;
    uint32_t file_len;

    zactor_t *actor;
} client_t;

client_t *client_new(void)
{
    client_t *client = (client_t*)malloc(sizeof(client_t));
    memset(client, 0, sizeof(client_t));

    return client;
}

void client_free(client_t *client)
{
    free(client);
}

void client_thread_main(zsock_t *pipe, void *user_data)
{
    client_t *client = (client_t*)user_data;
    int id = client->id;
    const char *file_data = client->file_data;
    uint32_t file_len = client->file_len;

    printf("==-== Client %d Ready.\n", id);

    zsock_signal(pipe, 0);

    char sz_id[16];
    sprintf(sz_id, "%d", id);

    zstr_send(pipe, ACTOR_READY);

    zsock_t *sock_client = zsock_new_req(edbroker_frontend_endpoint);
    uint32_t msg_count = 0;
    while ( true ){

        zmsg_t *msg = zmsg_new();
        zmsg_addmem(msg, file_data, file_len);
        zmsg_send(&msg, sock_client);

        zmsg_t *rsp = zmsg_recv(sock_client);
        if ( rsp == NULL ){
            zstr_send(pipe, ACTOR_OVER);
            break;
        }
        /*zmsg_print(rsp);*/
        zmsg_destroy(&rsp);

        msg_count++;
        if ( msg_count % 100 == 1 || msg_count + 5 >= MAX_COUNT ){
            printf("==-== Send message %d/%d\n", msg_count, MAX_COUNT);
        }
        if ( msg_count >= MAX_COUNT )
            break;
    }

    zstr_send(pipe, ACTOR_OVER);

    zsock_destroy(&sock_client);

    printf("==-== Client %d Exit.\n", id);

}

static int over_actors = 0;

int handle_read_on_client_pipe(zloop_t *loop, zsock_t *pipe, void *user_data)
{
    client_t *client = (client_t*)user_data;

    if ( over_actors >= NUM_ACTORS ){
        zloop_reader_end(loop, pipe);
        return -1;
    }

    zmsg_t *msg = zmsg_recv(pipe);
    if ( msg == NULL ){
        zloop_reader_end(loop, pipe);
        return -1;
    }

    /*zmsg_print(msg);*/

    char *actor_rsp = zmsg_popstr(msg);
    if ( strcmp(actor_rsp, ACTOR_OVER) == 0 ){
        over_actors++;
        printf("==-== Actor %d over! (%d/%d)\n", client->id, over_actors, NUM_ACTORS);
    }
    free(actor_rsp);

    zmsg_destroy(&msg);

    return 0;
}

static char file_data[1024 * 32];

int run_client(void)
{

    zactor_t *actors[NUM_ACTORS];
    client_t *clients[NUM_ACTORS];
    for ( int i = 0 ; i < NUM_ACTORS ; i++ ){
        client_t *client = client_new();
        client->id = i;
        client->file_data = file_data;
        client->file_len = 1024 * 32;
        zactor_t *actor = zactor_new(client_thread_main, client);
        client->actor = actor;

        actors[i] = actor;
        clients[i] = client;
    }

    zloop_t *loop = zloop_new();
    zloop_set_verbose(loop, 1);

    for ( int i = 0 ; i < NUM_ACTORS ; i++ ){
        zactor_t *actor = actors[i];
        zloop_reader(loop, (zsock_t*)zactor_resolve(actor), handle_read_on_client_pipe, clients[i]);
    }

    zloop_start(loop);

    zloop_destroy(&loop);

    for ( int i = 0 ; i < NUM_ACTORS ; i++ ){
        zactor_destroy(&actors[i]);
        client_free(clients[i]);
    }

    return 0;
}

