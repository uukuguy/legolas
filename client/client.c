/**
 * @file   client.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-05-05 20:54:54
 * 
 * @brief  
 * 
 * 
 */

#include "uv.h"
#include "common.h"
#include "work.h"
#include "logger.h"
#include "filesystem.h"
#include "message.h"
#include "client.h"
#include "crc32.h"
#include "md5.h"
#include "sysinfo.h"

#include "udb.h"

#define PREPARE_CLIENT \
    UNUSED client_runtime_t *client_runtime = (client_runtime_t*)udb_get_user_data(udb); \
    UNUSED client_t *client = client_runtime->client;

/* ==================== client_new() ==================== */ 
client_t *client_new(const char *ip, int port, int op_code, const char *key, const char *filename, int start_index, int total_files, int nthreads, int is_batch) 
{
    client_t *client = (client_t*)zmalloc(sizeof(client_t));
    memset(client, 0, sizeof(client_t));
    client->ip = ip;
    client->port = port;
    client->op_code = op_code;
    client->is_batch = is_batch;

    client->key_prefix = key;
    /*uint32_t keylen = strlen(key);*/
    /*memcpy(client->key, key, keylen);*/
    /*client->key[keylen] = '\0';*/

    client->filename = filename;
    client->start_index = start_index;
    client->total_files = total_files;
    client->nthreads = nthreads;
    client->write_request = NULL;

    pthread_mutex_init(&client->send_pending_lock, NULL);
    pthread_cond_init(&client->send_pending_cond, NULL);

    pthread_mutex_init(&client->recv_pending_lock, NULL);
    pthread_cond_init(&client->recv_pending_cond, NULL);

    return client;
}

/* ==================== client_free() ==================== */ 
void client_free(client_t *client){
    if ( client->file_data != NULL ){
        zfree(client->file_data);
        client->file_data = NULL;
    }

    pthread_mutex_destroy(&client->send_pending_lock);
    pthread_cond_destroy(&client->send_pending_cond);
    pthread_mutex_destroy(&client->recv_pending_lock);
    pthread_cond_destroy(&client->recv_pending_cond);

    zfree(client);
}


client_runtime_t *client_runtime_new(client_t *client)
{
    client_runtime_t *client_runtime = (client_runtime_t*)zmalloc(sizeof(client_runtime_t));
    memset(client_runtime, 0, sizeof(client_runtime_t));

    client_runtime->client = client;

    return client_runtime;
}

void client_runtime_free(client_runtime_t *client_runtime)
{
    if ( client_runtime != NULL ){
        zfree(client_runtime);
    }
}


void reset_udb_key_by_client(udb_t *udb, uint32_t total_processed)
{
    PREPARE_CLIENT;

    char file_name[NAME_MAX];
    get_path_file_name(client->filename, file_name, NAME_MAX - 1);

    char key[NAME_MAX];
    sprintf(key, "/test/%s/%04d/%08d-%s", client->key_prefix, udb->id, client->start_index + total_processed, file_name);

    uint32_t keylen = strlen(key);
    memcpy(udb->key, key, keylen);
    udb->keylen = keylen;

    udb->op_code = client->op_code;
    udb->object_size = client->file_size;
}

static int write_file(udb_t *udb);
static int read_file(udb_t *udb);
static int delete_file(udb_t *udb);

/* ==================== try_write_next_file() ==================== */ 
void try_write_next_file(udb_t *udb)
{
    PREPARE_CLIENT;
 
    if ( client_runtime->total_send < client->total_files) {

        if ( client_runtime->total_send % 100 == 0 ) {
            trace_log("\n-------- Session(%d) udb(%d) start to write %d/%d files", udb->session->id, udb->id, client_runtime->total_send, client->total_files);

            /*if ( client_runtime->total_send % 1000 == 0 ) {*/
                /*log_sysinfo();*/
            /*}*/
        }
        client_runtime->total_send++;

        write_file(udb);

    } else {
        if ( udb->is_batch  == 0 ){
            notice_log("\n===== Session(%d) udb(%d) Write %d files done. =====\n", udb->session->id, udb->id, client->total_files);

            udb_done(udb);
        }
    }
}

/* ==================== client_after_write_finished() ==================== */ 
int client_after_write_finished(udb_t *udb, message_t *response) 
{
    PREPARE_CLIENT;

    trace_log("after_write_finished() key: %s object_size: %d", udb->key, udb->object_size);

    if ( udb->is_batch == 0 ) {
        try_write_next_file(udb);
    } else {

        uint32_t total_finished = __sync_add_and_fetch(&client_runtime->total_finished, 1);
        if ( total_finished >=  client->total_files ){
            notice_log("\n===== Session(%d) udb(%d) Write %d files done. =====\n", udb->session->id, udb->id, client->total_files);
            udb_done(udb);
        } else {
            if ( total_finished % 100 == 1 || total_finished + 5 >= client->total_files) {
                notice_log("\n!!!----- Session(%d) udb(%d) finish write %d/%d files", udb->session->id, udb->id, total_finished, client->total_files);

                /*if ( total_finished % 1000 == 0 ) {*/
                /*log_sysinfo();*/
                /*}*/
            }
        }

    }

    return 0;
}

/* ==================== client_after_write_object_slice() ==================== */ 
int client_after_write_object_slice(udb_t *udb, msgidx_t *msgidx)
{
    PREPARE_CLIENT;

    /*trace_log("after_write_object_slice(). key: %s object_size: %d slice: %d/%d data_size:  %d", msgidx->key, msgidx->object_size, msgidx->slice_idx + 1, msgidx->nslices, msgidx->data_size);*/

    if ( !udb_is_write_done(udb) ){
        uint32_t total_writed = udb_get_writed_bytes(udb);
        char *buf = &client->file_data[total_writed];
        uint32_t block_size = DEFAULT_SOCKBUF_SIZE;

        UNUSED int ret;
        int handle = -1;
        ret = udb_append_data(udb, handle, buf, block_size);
    }

    return 0;
}

/* ==================== write_file() ==================== */ 
static int write_file(udb_t *udb)
{
    PREPARE_CLIENT;

    /* FIXME 2014-10-1t 05:08:19 */
    session_waiting_message(udb->session);

    reset_udb_key_by_client(udb, client_runtime->total_send);

    int handle = -1;
    char *buf = client->file_data;
    uint32_t block_size = DEFAULT_SOCKBUF_SIZE;
    UNUSED uint32_t writed = udb_write_data(udb, handle, buf, block_size, client_after_write_object_slice, client_after_write_finished);

    return 0;
}

/* ==================== try_read_next_file() ==================== */ 
void try_read_next_file(udb_t *udb)
{
    PREPARE_CLIENT;

    if ( client_runtime->total_recv < client->total_files - 1 ) {

        if ( client_runtime->total_recv % 100 == 0 ) {
            notice_log("\n-------- Session(%d) start to read %d/%d files", udb->id, client_runtime->total_recv, client->total_files);

            if ( client_runtime->total_recv % 1000 == 0 ) {
                log_sysinfo();
            }
        }

        client_runtime->total_recv++;
        read_file(udb);

    } else {

        notice_log("\n===== Session(%d) Read %d files done. =====\n", udb->id, client->total_files);

        udb_done(udb);
    }
}

/* ==================== client_after_read_finished() ==================== */ 
int client_after_read_finished(udb_t *udb, message_t *response) 
{
    trace_log("after_read_finished() key: %s object_size: %d", udb->key, udb->object_size);

    try_read_next_file(udb);

    return 0;
}

/* ==================== client_after_read_object_slice() ==================== */ 
int client_after_read_object_slice(udb_t *udb, msgidx_t *msgidx)
{
    trace_log("after_read_object_slice(). key: %s object_size: %d slice: %d/%d data_size:  %d", msgidx->key, msgidx->object_size, msgidx->slice_idx + 1, msgidx->nslices, msgidx->data_size);

    if ( msgidx->message->result != RESULT_SUCCESS ){
        try_read_next_file(udb);
    }

    return 0;
}

/* ==================== read_file() ==================== */ 
static int read_file(udb_t *udb)
{
    PREPARE_CLIENT;

    reset_udb_key_by_client(udb, client_runtime->total_recv);

    int handle = -1;
    udb_read_data(udb, handle, client_after_read_object_slice, client_after_read_finished);

    return 0;
}
 
/* ==================== try_delete_next_file() ==================== */ 
void try_delete_next_file(udb_t *udb)
{
    PREPARE_CLIENT;

    if ( client_runtime->total_del < client->total_files - 1) {

        if ( client_runtime->total_del % 100 == 0 ) {
            notice_log("\n-------- Session(%d) start to delete %d/%d files", udb->id, client_runtime->total_del, client->total_files);

            if ( client_runtime->total_del % 1000 == 0 ) {
                log_sysinfo();
            }
        }

        client_runtime->total_del++;
        delete_file(udb);

    } else {

        notice_log("\n===== Session(%d) Delete %d files done. =====\n", udb->id, client->total_files);

        udb_done(udb);
    }
}

/* ==================== client_after_delete_finished() ==================== */ 
int client_after_delete_finished(udb_t *udb, message_t *response) 
{
    trace_log("after_delete_finished() key:%s", udb->key);

    try_delete_next_file(udb);

    return 0;
}

/* ==================== delete_file() ==================== */ 
static int delete_file(udb_t *udb)
{
    PREPARE_CLIENT;

    reset_udb_key_by_client(udb, client_runtime->total_del);

    udb_delete_data(udb, client_after_delete_finished);

    return 0;
}

int client_execute(udb_t *udb)
{
    PREPARE_CLIENT;

    if ( client->op_code == MSG_OP_WRITE ) {
        write_file(udb);

    } else if ( client->op_code == MSG_OP_READ ) {
        read_file(udb);

    } else if ( client->op_code == MSG_OP_DEL ) {
        delete_file(udb);

    } else {
        warning_log("Uknown op_code: %d", client->op_code);
    }

    return 0;
}

/* ==================== test_run_task() ==================== */ 
int client_run_task(client_t *client, int id)
{
    int ret = 0;

    client_runtime_t *client_runtime = client_runtime_new(client);

    udb_t *udb = udb_new(client->ip, client->port, (void*)client_runtime);
    udb->is_batch = client->is_batch;

    ret = udb_do(udb, client_execute);


    udb_free(udb);

    return ret;
}

