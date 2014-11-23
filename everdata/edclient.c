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

#include "zpipe.h"

static char *file_data = NULL;
static uint32_t file_size = 0;

/* -------- struct client_t -------- */
typedef struct client_t {
    ZPIPE_ACTOR;

    const char *endpoint;
    int id;
    const char *file_data;
    uint32_t file_size;
    uint32_t total_files;
    int op_code;
    const char *key;
    const char *filename;
    int verbose;

    uint32_t start_index;
    /*zactor_t *actor;*/

} client_t;

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

/* ================ client_rebuild_data_key() ================ */
void client_rebuild_data_key(client_t *client, uint32_t data_id, char *key)
{
    char file_name[NAME_MAX];
    get_path_file_name(client->filename, file_name, NAME_MAX - 1);

    sprintf(key, "/test/%s/%04d/%08d-%s", client->key, client->id, client->start_index + data_id, file_name);
}

/* ================ delete_data() ================ */
int delete_data(zsock_t *sock, const char *key)
{
    /* ---------------- Send Message ---------------- */
    zmsg_t *delete_msg = create_action_message(MSG_ACTION_DEL);
    message_add_key_data(delete_msg, key, "", 0);

    zmsg_send(&delete_msg, sock);

    /* ---------------- Receive Message ---------------- */

    zmsg_t *recv_msg = zmsg_recv(sock);
    if ( recv_msg == NULL ){
        return -2;
    }
    zmsg_print(recv_msg);

    int rc = -1;
    if (message_check_status(recv_msg, MSG_STATUS_WORKER_NOTFOUND) == 0 ){
        warning_log("Not Found. key=%s", key);
        rc = 0;
    } else if ( message_check_status(recv_msg, MSG_STATUS_WORKER_ERROR) == 0 ){
        error_log("Return MSG_STATUS_WORKER_ERROR. key=%s", key);
        rc = -1;
    }

    zmsg_destroy(&recv_msg);

    return rc;
}

/* ================ download_data() ================ */
int download_data(zsock_t *sock, const char *key)
{
    /* ---------------- Send Message ---------------- */
    zmsg_t *download_msg = create_action_message(MSG_ACTION_GET);
    message_add_key_data(download_msg, key, "", 0);

    zmsg_send(&download_msg, sock);

    /* ---------------- Receive Message ---------------- */

    zmsg_t *recv_msg = zmsg_recv(sock);
    if ( recv_msg == NULL ){
        return -2;
    }
    /*zmsg_print(recv_msg);*/

    int rc = -1;
    if (message_check_status(recv_msg, MSG_STATUS_WORKER_NOTFOUND) == 0 ){
        warning_log("Not Found. key=%s", key);
        rc = 0;
    } else if ( message_check_status(recv_msg, MSG_STATUS_WORKER_ERROR) == 0 ){
        error_log("Return MSG_STATUS_WORKER_ERROR. key=%s", key);
        rc = -1;
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
                rc = 0;
            }
        }
    }

    zmsg_destroy(&recv_msg);

    return rc;
}

/* ================ upload_data() ================ */
int upload_data(zsock_t *sock, const char *key, const char *data, uint32_t data_size)
{
    /* ---------------- Send Message ---------------- */
    zmsg_t *upload_msg = create_action_message(MSG_ACTION_PUT);
    message_add_key_data(upload_msg, key, data, data_size);

    zmsg_send(&upload_msg, sock);

    /* ---------------- Receive Message ---------------- */

    zmsg_t *recv_msg = zmsg_recv(sock);
    if ( recv_msg == NULL ){
        return -2;
    }
    /*zmsg_print(recv_msg);*/

    int rc = 0;
    if (message_check_status(recv_msg, MSG_STATUS_WORKER_ACK) == 0 ){
        /*info_log("Return MSG_STATUS_WORKER_ACK. key=%s", key);*/
        rc = 0;
    } else if ( message_check_status(recv_msg, MSG_STATUS_WORKER_ERROR) == 0 ){
        error_log("Return MSG_STATUS_WORKER_ERROR. key=%s", key);
        rc = -1;
    }

    zmsg_destroy(&recv_msg);

    return rc;
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

    ZPIPE_ACTOR_THREAD_BEGIN(pipe);

    zsock_t *sock_client = zsock_new_req(client->endpoint);
    if ( sock_client == NULL ){
        error_log("Connect broker failed. Client %d", client->id);
        return;
    }

    uint32_t file_count = 0;
    while ( true ){

        char key[NAME_MAX];
        client_rebuild_data_key(client, file_count, key);

        int retries = 0;
        while ( retries++ < RETRIES ) {

            int rc = 0;
            if ( client->op_code == 1 ) {
                rc = upload_data(sock_client, key, file_data, file_size);
            } else if ( client->op_code == 2 ) {
                rc = download_data(sock_client, key);
            } else if ( client->op_code == 3 ) {
                rc = delete_data(sock_client, key);
            }
            if ( rc == -2 ) break;
            if ( rc == 0 ) break;

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

    ZPIPE_ACTOR_THREAD_END(pipe);

    zsock_destroy(&sock_client);

    trace_log("Client %d Exit.", id);

}

/* ================ client_new() ================ */
client_t *client_new(int client_id, const char *endpoint, int op_code, uint32_t total_files, const char *key, const char *filename, int verbose)
{
    client_t *client = (client_t*)malloc(sizeof(client_t));
    memset(client, 0, sizeof(client_t));

    client->id = client_id;
    client->endpoint = endpoint;
    client->op_code = op_code;
    client->total_files = total_files;
    client->key = key;
    client->filename = filename;
    client->verbose = verbose;

    client->file_data = file_data;
    client->file_size = file_size;

    ZPIPE_ACTOR_NEW(client, client_thread_main);

    return client;
}

/* ================ client_free() ================ */
void client_free(client_t *client)
{
    ZPIPE_ACTOR_FREE(client);
    free(client);
}

/* -------- struct edclient_t -------- */
typedef struct edclient_t {
    ZPIPE;

    const char *endpoint;
    int op_code;
    uint32_t total_clients;
    uint32_t total_files;
    const char *key;
    const char *filename;
    int verbose;
} edclient_t;

/* ================ edclient_new() ================ */
edclient_t *edclient_new(const char *endpoint, int op_code, uint32_t total_clients, uint32_t total_files, const char *key, const char *filename, int verbose)
{
    edclient_t *edclient = (edclient_t*)malloc(sizeof(edclient_t));
    memset(edclient, 0, sizeof(edclient_t));

    edclient->endpoint = endpoint;
    edclient->op_code = op_code;
    edclient->total_clients = total_clients;
    edclient->total_files = total_files;
    edclient->key = key;
    edclient->filename = filename;
    edclient->verbose = verbose;

    return edclient;
}

/* ================ edclient_free() ================ */
void edclient_free(edclient_t *edclient)
{
    ZPIPE_FREE(edclient, client_free);

    free(edclient);
}

/* ================ edclient_loop() ================ */
void edclient_loop(edclient_t *edclient){

    ZPIPE_NEW_BEGIN(edclient, edclient->total_clients);

    client_t *client = client_new(i, 
            edclient->endpoint, 
            edclient->op_code, 
            edclient->total_files, 
            edclient->key, 
            edclient->filename, 
            edclient->verbose);

    ZPIPE_NEW_END(edclient, client);

    ZPIPE_LOOP(edclient);
}

/* ================ run_edclient() ================ */
int run_edclient(const char *endpoint, int op_code, uint32_t total_clients, uint32_t total_files, const char *key, const char *filename, int verbose)
{
    edclient_t *edclient = edclient_new(endpoint, op_code, total_clients, total_files, key, filename, verbose);

    edclient_loop(edclient);

    edclient_free(edclient);

    return 0;
}

