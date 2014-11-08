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
#include "logger.h"

#define ACTOR_READY "ACTOR READY"
#define ACTOR_OVER "ACTOR OVER"

typedef struct client_t {
    const char *endpoint;
    int id;
    const char *file_data;
    uint32_t file_size;
    uint32_t total_threads;
    uint32_t total_files;
    int op_code;
    const char *key;
    const char *filename;

    zactor_t *actor;

} client_t;

void client_thread_main(zsock_t *pipe, void *user_data)
{
    client_t *client = (client_t*)user_data;
    int id = client->id;
    const char *file_data = client->file_data;
    uint32_t file_size = client->file_size;

    trace_log("Client %d Ready.", id);

    zsock_signal(pipe, 0);

    char sz_id[16];
    sprintf(sz_id, "%d", id);

    zstr_send(pipe, ACTOR_READY);

    zsock_t *sock_client = zsock_new_req(client->endpoint);
    uint32_t msg_count = 0;
    while ( true ){

        zmsg_t *msg = zmsg_new();
        zmsg_addmem(msg, file_data, file_size);
        /*zmsg_print(msg);*/
        zmsg_send(&msg, sock_client);

        zmsg_t *rsp = zmsg_recv(sock_client);
        if ( rsp == NULL ){
            zstr_send(pipe, ACTOR_OVER);
            break;
        }
        /*zmsg_print(rsp);*/
        zmsg_destroy(&rsp);

        msg_count++;
        if ( msg_count % 100 == 1 || msg_count + 5 >= client->total_files ){
            info_log("Send message %d/%d", msg_count, client->total_files);
        }
        if ( msg_count >= client->total_files )
            break;
    }

    zstr_send(pipe, ACTOR_OVER);

    zsock_destroy(&sock_client);

    trace_log("Client %d Exit.", id);

}

client_t *client_new(int client_id, const char *endpoint)
{
    client_t *client = (client_t*)malloc(sizeof(client_t));
    memset(client, 0, sizeof(client_t));

    client->id = client_id;
    client->endpoint = endpoint;

    return client;
}

void client_create_actor(client_t *client)
{
    client->actor = zactor_new(client_thread_main, client);
}

void client_free(client_t *client)
{
    zactor_destroy(&client->actor);
    client->actor = NULL;
    free(client);
}

static int over_actors = 0;
static uint32_t total_actors = 0;

int handle_read_on_client_pipe(zloop_t *loop, zsock_t *pipe, void *user_data)
{
    client_t *client = (client_t*)user_data;

    if ( over_actors >= total_actors ){
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
        info_log("Actor %d over! (%d/%d)", client->id, over_actors, total_actors);
    }
    free(actor_rsp);

    zmsg_destroy(&msg);

    return 0;
}

static char *file_data = NULL;
static uint32_t file_size = 0;

int prepare_file_data(const char *filename)
{
    FILE *file = fopen(filename, "rb");
    if ( file == NULL ){
        error_log("fopen() failed. file:%s", filename);
        return -1;
    }

    fseek(file, 0, SEEK_END);
    uint32_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *buf = zmalloc(size);
    /*memset(buf, 'Z', sizeof(size));*/

    memset(buf, 0, sizeof(size));
    uint32_t readed = fread(buf, 1, size, file); 
    if ( readed != size ){
        error_log("fread() failed. readed:%d file_size:%d", readed, size);
        fclose(file);
        return -1;
    }

    fclose(file);

    file_data = buf;
    file_size = size;

    return 0;
}

int run_client(const char *endpoint, int op_code, int total_threads, uint32_t total_files, const char *key, const char *filename, int verbose)
{
    info_log("run_client() with op_code:%d endpoint:%s threads:%d count:%d key:%s filename:%s", op_code, endpoint, total_threads, total_files, key, filename);

    if ( prepare_file_data(filename) != 0 ){
        return -1;
    }

    total_actors = total_threads;

    client_t **clients = (client_t**)malloc(sizeof(client_t*) * total_actors);
    for ( int i = 0 ; i < total_actors ; i++ ){
        client_t *client = client_new(i, endpoint);

        client->op_code = op_code;
        client->total_threads = total_threads;
        client->total_files = total_files;
        client->key = key;
        client->filename = filename;

        client->file_data = file_data;
        client->file_size = file_size;

        client_create_actor(client);

        clients[i] = client;
    }

    zloop_t *loop = zloop_new();
    zloop_set_verbose(loop, verbose);

    for ( int i = 0 ; i < total_actors ; i++ ){
        zactor_t *actor = clients[i]->actor;
        zloop_reader(loop, (zsock_t*)zactor_resolve(actor), handle_read_on_client_pipe, clients[i]);
    }

    zloop_start(loop);

    zloop_destroy(&loop);

    for ( int i = 0 ; i < total_actors ; i++ ){
        client_free(clients[i]);
    }

    return 0;
}

