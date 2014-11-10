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
#include "filesystem.h"
#include "everdata.h"

/* -------- struct client_t -------- */
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

    uint32_t start_index;
    zactor_t *actor;

} client_t;

/* ================ client_rebuild_data_key() ================ */
void client_rebuild_data_key(client_t *client, uint32_t data_id, char *key)
{
    char file_name[NAME_MAX];
    get_path_file_name(client->filename, file_name, NAME_MAX - 1);

    sprintf(key, "/test/%s/%04d/%08d-%s", client->key, client->id, client->start_index + data_id, file_name);
}

#define RETRIES 3
/* ================ client_thread_main() ================ */
void client_thread_main(zsock_t *pipe, void *user_data)
{
    client_t *client = (client_t*)user_data;
    int id = client->id;
    UNUSED const char *file_data = client->file_data;
    UNUSED uint32_t file_size = client->file_size;

    trace_log("Client %d Ready.", id);

    zsock_signal(pipe, 0);

    char sz_id[16];
    sprintf(sz_id, "%d", id);

    message_send_status(pipe, MSG_STATUS_ACTOR_READY);

    zsock_t *sock_client = zsock_new_req(client->endpoint);
    uint32_t file_count = 0;
    while ( true ){

        char key[NAME_MAX];
        client_rebuild_data_key(client, file_count, key);

        int Ok = 0;
        int retries = 0;
        while ( retries++ < RETRIES ){

            /* ---------------- Send Message ---------------- */
            if ( client->op_code == 1 ){
                zmsg_t *upload_msg = create_action_message(MSG_ACTION_PUT);
                message_add_key_data(upload_msg, key, file_data, file_size);

                zmsg_send(&upload_msg, sock_client);
            } else if ( client->op_code == 2 ) {

                zmsg_t *download_msg = create_action_message(MSG_ACTION_GET);
                message_add_key_data(download_msg, key, "", 0);

                zmsg_send(&download_msg, sock_client);
            } else if ( client->op_code == 3 ) {
                zmsg_t *delete_msg = create_action_message(MSG_ACTION_DEL);
                message_add_key_data(delete_msg, key, "", 0);

                zmsg_send(&delete_msg, sock_client);
            }

            /* ---------------- Receive Message ---------------- */

            zmsg_t *recv_msg = zmsg_recv(sock_client);
            if ( recv_msg == NULL ){
                /*zstr_send(pipe, ACTOR_OVER);*/
                message_send_status(pipe, MSG_STATUS_ACTOR_OVER);
                break;
            }
            /*zmsg_print(recv_msg);*/

            if (message_check_status(recv_msg, MSG_STATUS_WORKER_ACK) == 0 ){
                /*info_log("Return MSG_STATUS_WORKER_ACK. key=%s", key);*/
                Ok = 1;
            } else if ( message_check_status(recv_msg, MSG_STATUS_WORKER_ERROR) == 0 ){
                error_log("Return MSG_STATUS_WORKER_ERROR. key=%s", key);
            } else {
                /*zmsg_print(recv_msg);*/
                zframe_t *frame_msgtype = zmsg_first(recv_msg);
                if ( frame_msgtype != NULL ){
                    int16_t msgtype = *(int16_t*)zframe_data(frame_msgtype);
                    if ( msgtype == MSGTYPE_DATA ){
                        zmsg_first(recv_msg);

                        zframe_t *frame_key = zmsg_next(recv_msg);
                        UNUSED const char *key = (const char *)zframe_data(frame_key);

                        zframe_t *frame_data = zmsg_next(recv_msg);
                        UNUSED const char *data =  (const char *)zframe_data(frame_data);
                        UNUSED uint32_t data_size = zframe_size(frame_data);
                        /*notice_log("Receive key:%s data_size:%d", key, data_size);*/
                        Ok = 1;
                    }
                }
            }

            zmsg_destroy(&recv_msg);

            if ( Ok == 1 ) break;
            notice_log("Retry %d/%d...", retries, RETRIES);
            zclock_sleep(1000);
        }

        /* ---------------- Check exit loop ---------------- */

        file_count++;
        if ( file_count % 100 == 1 || file_count + 5 >= client->total_files ){
            info_log("Client %d Send message %d/%d", client->id, file_count, client->total_files);
        }
        if ( file_count >= client->total_files )
            break;
    }

    message_send_status(pipe, MSG_STATUS_ACTOR_OVER);

    zsock_destroy(&sock_client);

    trace_log("Client %d Exit.", id);

}

/* ================ client_new() ================ */
client_t *client_new(int client_id, const char *endpoint)
{
    client_t *client = (client_t*)malloc(sizeof(client_t));
    memset(client, 0, sizeof(client_t));

    client->id = client_id;
    client->endpoint = endpoint;

    return client;
}

/* ================ client_create_actor() ================ */
void client_create_actor(client_t *client)
{
    client->actor = zactor_new(client_thread_main, client);
    notice_log("Client %d start actor.", client->id);
}

/* ================ client_free() ================ */
void client_free(client_t *client)
{
    zactor_destroy(&client->actor);
    client->actor = NULL;
    free(client);
}

static int over_actors = 0;
static uint32_t total_actors = 0;

/* ================ handle_pullin_on_client_pipe() ================ */
int handle_pullin_on_client_pipe(zloop_t *loop, zsock_t *pipe, void *user_data)
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

    if ( message_check_status(msg, MSG_STATUS_ACTOR_OVER) == 0 ){
        over_actors++;
        info_log("Actor %d over! (%d/%d)", client->id, over_actors, total_actors);
    }

    zmsg_destroy(&msg);

    return 0;
}

static char *file_data = NULL;
static uint32_t file_size = 0;

/* ================ prepare_file_data() ================ */
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

/* ================ run_client() ================ */
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
        zloop_reader(loop, (zsock_t*)zactor_resolve(actor), handle_pullin_on_client_pipe, clients[i]);
    }

    zloop_start(loop);

    zloop_destroy(&loop);

    for ( int i = 0 ; i < total_actors ; i++ ){
        client_free(clients[i]);
    }

    return 0;
}

