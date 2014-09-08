/**
 * @file   test.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-09-09 21:28:49
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

#include "udclient.h"

void reset_udclient_key_by_client(udclient_t *udcli, uint32_t total_processed)
{
    client_t *client = (client_t*)udcli->user_data;

    char file_name[NAME_MAX];
    get_path_file_name(client->filename, file_name, NAME_MAX - 1);

    char key[NAME_MAX];
    sprintf(key, "/test/%s/%04d/%08d-%s", client->key_prefix, udcli->id, udcli->start_index + total_processed, file_name);

    uint32_t keylen = strlen(key);
    memcpy(udcli->key, key, keylen);
    udcli->keylen = keylen;

    udcli->total_writed = 0;
    udcli->total_readed = 0;
}

static int write_file(udclient_t *udcli);
static int read_file(udclient_t *udcli);
static int delete_file(udclient_t *udcli);

void try_write_next_file(udclient_t *udcli)
{
    if ( udcli->total_send < udcli->total_files - 1 ) {
        if ( udcli->total_send % 100 == 0 ) {
            notice_log("\n-------- Session(%d) start to write %d/%d files", udcli->id, udcli->total_send, udcli->total_files);
        }

        udcli->total_send++;
        reset_udclient_key_by_client(udcli, udcli->total_send);
        write_file(udcli);
    } else {
        notice_log("\n===== Session(%d) Write %d files done. =====\n", udcli->id, udcli->total_files);
        pthread_cond_signal(&udcli->main_pending_cond);
    }
}

/* ==================== test_after_write_finished() ==================== */ 
int test_after_write_finished(udclient_t *udcli, message_t *response) 
{
    trace_log("after_write_finished() key: %s object_size: %d", udcli->key, udcli->object_size);

    try_write_next_file(udcli);

    return 0;
}

/* ==================== test_after_write_object_slice() ==================== */ 
int test_after_write_object_slice(udclient_t *udcli, msgidx_t *msgidx)
{
    client_t *client = (client_t*)udcli->user_data;
    trace_log("after_write_object_slice(). key: %s object_size: %d slice: %d/%d data_size:  %d", msgidx->key, msgidx->object_size, msgidx->slice_idx + 1, msgidx->nslices, msgidx->data_size);
    if ( udcli->total_writed < udcli->object_size ){
        char *buf = &client->file_data[udcli->total_writed];
        uint32_t block_size = DEFAULT_SOCKBUF_SIZE;

        UNUSED int ret;
        int handle = -1;
        ret = udclient_write_data(udcli, handle, buf, block_size);
    }

    return 0;
}

/* ==================== write_file() ==================== */ 
static int write_file(udclient_t *udcli)
{
    client_t *client = (client_t*)udcli->user_data;

    int handle = -1;
    char *buf = client->file_data;
    uint32_t block_size = DEFAULT_SOCKBUF_SIZE;
    UNUSED uint32_t writed = udclient_write_data(udcli, handle, buf, block_size);

    /*uint32_t file_data_sended = 0;*/

    /*while ( file_data_sended < client->file_size ){*/
        /*char *buf = &client->file_data[file_data_sended];*/
        /*uint32_t block_size = DEFAULT_SOCKBUF_SIZE;*/

        /*UNUSED int ret;*/
        /*ret = udclient_write_data(udcli, handle, buf, block_size);*/

        /*file_data_sended += block_size;*/
    /*};*/

    return 0;
}

void try_read_next_file(udclient_t *udcli)
{
    if ( udcli->total_recv < udcli->total_files - 1 ) {
        if ( udcli->total_recv % 100 == 0 ) {
            notice_log("\n-------- Session(%d) start to read %d/%d files", udcli->id, udcli->total_recv, udcli->total_files);
        }
        udcli->total_recv++;
        reset_udclient_key_by_client(udcli, udcli->total_recv);
        read_file(udcli);
    } else {
        notice_log("\n===== Session(%d) Read %d files done. =====\n", udcli->id, udcli->total_files);
        pthread_cond_signal(&udcli->main_pending_cond);
    }
}

/* ==================== test_after_read_finished() ==================== */ 
int test_after_read_finished(udclient_t *udcli, message_t *response) 
{
    trace_log("after_read_finished() key: %s object_size: %d", udcli->key, udcli->object_size);

    try_read_next_file(udcli);

    return 0;
}

/* ==================== test_after_read_object_slice() ==================== */ 
int test_after_read_object_slice(udclient_t *udcli, msgidx_t *msgidx)
{
    trace_log("after_read_object_slice(). key: %s object_size: %d slice: %d/%d data_size:  %d", msgidx->key, msgidx->object_size, msgidx->slice_idx + 1, msgidx->nslices, msgidx->data_size);

    if ( msgidx->message->result != RESULT_SUCCESS ){
        try_read_next_file(udcli);
    }

    return 0;
}

/* ==================== read_file() ==================== */ 
static int read_file(udclient_t *udcli)
{
    int handle = -1;
    char *buf = NULL;
    uint32_t block_size = 0;
    udclient_read_data(udcli, handle, buf, block_size);

    return 0;
}

/* ==================== test_after_delete_finished() ==================== */ 
int test_after_delete_finished(udclient_t *udcli, message_t *response) 
{
    trace_log("after_delete_finished() key:%s", udcli->key);

    if ( udcli->total_del < udcli->total_files - 1) {
        if ( udcli->total_del % 100 == 0 ) {
            notice_log("\n-------- Session(%d) start to delete %d/%d files", udcli->id, udcli->total_del, udcli->total_files);
        }
        udcli->total_del++;
        reset_udclient_key_by_client(udcli, udcli->total_del);
        delete_file(udcli);
    } else {
        notice_log("\n===== Session(%d) Delete %d files done. =====\n", udcli->id, udcli->total_files);
        pthread_cond_signal(&udcli->main_pending_cond);
    }
    return 0;
}

/* ==================== delete_file() ==================== */ 
static int delete_file(udclient_t *udcli)
{
    udclient_delete_data(udcli);
    return 0;
}

int test_first_file(udclient_t *udcli)
{
    reset_udclient_key_by_client(udcli, 0);

    client_t *client = (client_t*)udcli->user_data;

    if ( udcli->op_code == MSG_OP_WRITE ) {
        udcli->total_send = 0;
        udcli->object_size = client->file_size;
        write_file(udcli);

    } else if ( udcli->op_code == MSG_OP_READ ) {
        udcli->total_recv = 0;
        read_file(udcli);

    } else if ( udcli->op_code == MSG_OP_DEL ) {
        delete_file(udcli);

    } else {
        warning_log("Uknown op_code: %d", udcli->op_code);
    }

    return 0;
}

int test_on_ready(udclient_t *udcli)
{
    test_first_file(udcli);
    return 0;
}

int test_init(udclient_t *udcli)
{
    udcli->on_ready = test_on_ready;
    udcli->after_write_finished = test_after_write_finished;
    udcli->after_read_finished = test_after_read_finished;
    udcli->after_delete_finished = test_after_delete_finished;
    
    udcli->after_write_object_slice = test_after_write_object_slice;
    udcli->after_read_object_slice = test_after_read_object_slice;

    return 0;
}

/* ==================== test() ==================== */ 
int test(udclient_t *udcli)
{
    test_first_file(udcli);

    /*char file_name[NAME_MAX];*/
    /*get_path_file_name(client->filename, file_name, NAME_MAX - 1);*/
    
    /*uint32_t i;*/
    /*for ( i = 0 ; i < udcli->total_files ; i++ ){*/
        /*char key[NAME_MAX];*/
        /*sprintf(key, "/test/%s/%04d/%08d-%s", client->key_prefix, udcli->id, udcli->start_index + i, file_name);*/

        /*if ( udcli->op_code == MSG_OP_WRITE ) {*/
            /*udcli->object_size = udcli->file_size;*/
            /*write_file(udcli);*/

        /*} else if ( udcli->op_code == MSG_OP_READ ) {*/
            /*read_file(udcli);*/

        /*} else if ( udcli->op_code == MSG_OP_DEL ) {*/
            /*delete_file(udcli);*/

        /*} else {*/
            /*warning_log("Uknown op_code: %d", udcli->op_code);*/
        /*}*/

    /*}*/


    return 0;
}

int test_run_task(client_t *client, int id)
{

    udclient_t *udcli = udclient_new((void*)client);

    test_init(udcli);
    udcli->id = id;
    udcli->op_code = client->op_code;
    udcli->start_index = client->start_index;
    udcli->total_files = client->total_files;


	pthread_mutex_init(&udcli->main_pending_lock, NULL);
	pthread_cond_init(&udcli->main_pending_cond, NULL);

    udclient_run(udcli);

    int ret = 0;
    /*ret = test(udcli);*/

    pthread_mutex_lock(&udcli->main_pending_lock);
    pthread_cond_wait(&udcli->main_pending_cond, &udcli->main_pending_lock);
    pthread_mutex_unlock(&udcli->main_pending_lock);

    pthread_mutex_destroy(&udcli->main_pending_lock);
    pthread_cond_destroy(&udcli->main_pending_cond);

    udclient_free(udcli);

    return ret;
}

