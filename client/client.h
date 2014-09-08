/**
 * @file   client.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-07-07 01:50:09
 * 
 * @brief  
 * 
 * 
 */

#ifndef __CLIENT_H__
#define __CLIENT_H__ 

#include "zmalloc.h"
#include "list.h"
#include "adlist.h"
#include "uv.h"
#include "work.h"
#include "logger.h"
#include "message.h"
#include "session.h"

typedef struct client_args_t {
    int session_id;
    int op_code;

    uint32_t file_size;
    char *file_data;
    uint32_t file_data_sended;

    //FILE* file;
    int start_index;
    int total_send;
    int total_recv;
    char key[NAME_MAX];

    uv_connect_t connect_req;

    uint32_t file_opened;
    uint32_t file_closed;
} client_args_t;

typedef struct client_t {
    const char *ip;
    int port;

    int op_code;

    char *file_data;
    uint32_t file_size;

    //uint32_t total_readed;
    const char *filename;
    const char *key_prefix;

    uint32_t start_index;
    uint32_t total_files;
    uint32_t total_send;
    uint32_t total_recv;
    uint32_t total_del;
    int nthreads;

    void *write_request;
    pthread_mutex_t send_pending_lock;
    pthread_cond_t send_pending_cond;


    void *read_request;
    pthread_mutex_t recv_pending_lock;
    pthread_cond_t recv_pending_cond;


} client_t;

#define CLIENT(session) (client_t*)(session->parent)
#define CLIENT_ARGS(session) (client_args_t*)(session->user_data)

client_t *client_new(const char *ip, int port, int op_code, const char *key, const char *file, int start_index, int total_files, int nthreads);
void client_free(client_t *client);

#endif /* __CLIENT_H__ */

