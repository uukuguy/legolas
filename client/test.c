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

static int write_file(udclient_t *udcli);
/* ==================== test_after_write_finished() ==================== */ 
int test_after_write_finished(udclient_t *udcli, message_t *response) 
{
    client_t *client = (client_t*)udcli->user_data;

    trace_log("after_write_finished() key: %s object_size: %d", udcli->key, udcli->object_size);

    if ( client->total_send < client->total_files ) {
        client->total_send++;
        write_file(udcli);
    }

    return 0;
}

/* ==================== test_after_write_object_slice() ==================== */ 
int test_after_write_object_slice(udclient_t *udcli, msgidx_t *msgidx)
{
    client_t *client = (client_t*)udcli->user_data;
    trace_log("after_write_object_slice(). key: %s object_size: %d slice: %d/%d data_size:  %d", msgidx->key, msgidx->object_size, msgidx->slice_idx, msgidx->nslices, msgidx->data_size);
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

    char file_name[NAME_MAX];
    get_path_file_name(client->filename, file_name, NAME_MAX - 1);

    char key[NAME_MAX];
    sprintf(key, "/test/%s/%04d/%08d-%s", client->key_prefix, udcli->id, client->start_index + client->total_send, file_name);

    uint32_t keylen = strlen(key);
    memcpy(udcli->key, key, keylen);
    udcli->keylen = keylen;

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

/* ==================== test_after_read_finished() ==================== */ 
int test_after_read_finished(udclient_t *udcli, message_t *response) 
{
    trace_log("after_read_finished() key: %s object_size: %d", udcli->key, udcli->object_size);

    return 0;
}

/* ==================== test_after_read_object_slice() ==================== */ 
int test_after_read_object_slice(udclient_t *udcli, msgidx_t *msgidx)
{
    trace_log("after_read_object_slice(). key: %s object_size: %d slice: %d/%d data_size:  %d", msgidx->key, msgidx->object_size, msgidx->slice_idx, msgidx->nslices, msgidx->data_size);

    return 0;
}

/* ==================== read_file() ==================== */ 
static int read_file(udclient_t *udcli)
{
    return 0;
}

/* ==================== test_after_delete_finished() ==================== */ 
int test_after_delete_finished(udclient_t *udcli, message_t *response) 
{
    trace_log("after_delete_finished() key:%s", udcli->key);

    return 0;
}

/* ==================== delete_file() ==================== */ 
static int delete_file(udclient_t *udcli)
{
    return 0;
}

int test_first_file(client_t *client, udclient_t *udcli)
{
    if ( client->op_code == MSG_OP_WRITE ) {
        client->total_send = 0;
        udcli->object_size = client->file_size;
        write_file(udcli);

    } else if ( client->op_code == MSG_OP_READ ) {
        client->total_recv = 0;
        read_file(udcli);

    } else if ( client->op_code == MSG_OP_DEL ) {
        delete_file(udcli);

    } else {
        warning_log("Uknown op_code: %d", client->op_code);
    }

    return 0;
}

/* ==================== test() ==================== */ 
int test(client_t *client, udclient_t *udcli)
{
    udcli->after_write_finished = test_after_write_finished;
    udcli->after_read_finished = test_after_read_finished;
    udcli->after_delete_finished = test_after_delete_finished;
    
    udcli->after_write_object_slice = test_after_write_object_slice;
    udcli->after_read_object_slice = test_after_read_object_slice;

    test_first_file(client, udcli);

    /*char file_name[NAME_MAX];*/
    /*get_path_file_name(client->filename, file_name, NAME_MAX - 1);*/
    
    /*uint32_t i;*/
    /*for ( i = 0 ; i < client->total_files ; i++ ){*/
        /*char key[NAME_MAX];*/
        /*sprintf(key, "/test/%s/%04d/%08d-%s", client->key_prefix, udcli->id, client->start_index + i, file_name);*/

        /*if ( client->op_code == MSG_OP_WRITE ) {*/
            /*udcli->object_size = client->file_size;*/
            /*write_file(udcli);*/

        /*} else if ( client->op_code == MSG_OP_READ ) {*/
            /*read_file(udcli);*/

        /*} else if ( client->op_code == MSG_OP_DEL ) {*/
            /*delete_file(udcli);*/

        /*} else {*/
            /*warning_log("Uknown op_code: %d", client->op_code);*/
        /*}*/

    /*}*/


    return 0;
}

