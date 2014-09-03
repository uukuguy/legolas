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
#include "message.h"
#include "client.h"
#include "crc32.h"
#include "md5.h"

#include "udclient.h"

/* ==================== test_after_write_finished() ==================== */ 
int test_after_write_finished(udclient_t *udcli, message_t *response) 
{
    trace_log("after_write_finished() key:%s", udcli->key);

    return 0;
}

/* ==================== write_file() ==================== */ 
int write_file(client_t *client, udclient_t *udcli, const char *key)
{
    int handle = -1;
    const char *data;
    uint32_t data_len;

    uint32_t file_data_sended = 0;

    while ( file_data_sended < client->file_size ){
        char *buf = &client->file_data[file_data_sended];
        uint32_t block_size = DEFAULT_SOCKBUF_SIZE - head_size - sizeof(uint32_t); /* data size*/
        block_size -= block_size % 512;

        int ret;
        ret = udclient_write_data(udcli, handle, buf, block_size);

        file_data_sended += block_size;
    };

    return ret;
}

/* ==================== test_after_read_finished() ==================== */ 
int test_after_read_finished(udclient_t *udcli, message_t *response) 
{
    trace_log("after_read_finished() key:%s", udcli->key);

    return 0;
}

/* ==================== read_file() ==================== */ 
int read_file(client_t *client, udclient_t *udcli, const char *key)
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
int delete_file(client_t *client, udclient_t *udcli, const char *key)
{
    return 0;
}

/* ==================== test() ==================== */ 
int test(client_t *client, udclient_t *udcli, int session_id)
{

    udcli->after_write_finished = test_after_write_finished;
    udcli->after_read_finished = test_after_read_finished;
    udcli->after_delete_finished = test_after_delete_finished;

    char file_name[NAME_MAX];
    get_path_file_name(client->filename, file_name, NAME_MAX - 1);
    
    uint32_t i;
    for ( i = 0 ; i < client->total_files ; i++ ){
        char key[NAME_MAX];
        sprintf(key, "/test/%s/%04d/%08d-%s", client->key_prefix, session_id, client->start_index + i, file_name);

        if ( client->op_code == MSG_OP_WRITE ) {
            write_file(client, udcli, key);
        } else if ( client->op_code == MSG_OP_READ ) {
            read_file(client, udcli, key);
        } else if ( client->op_code == MSG_OP_DEL ) {
            delete_file(client, udcli, key);
        } else {
            warning_log("Uknown op_code: %d", client->op_code);
        }

    }


    return 0;
}

