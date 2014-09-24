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
#include "message.h"
#include "client.h"
#include "crc32.h"
#include "md5.h"


/* ==================== client_new() ==================== */ 
client_t *client_new(const char *ip, int port, int op_code, const char *key, const char *filename, int start_index, int total_files, int nthreads) 
{
    client_t *client = (client_t*)zmalloc(sizeof(client_t));
    memset(client, 0, sizeof(client_t));
    client->ip = ip;
    client->port = port;
    client->op_code = op_code;

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


/* ==================== on_close() ==================== */ 
/*void on_close(uv_handle_t* handle) {*/
    /*session_t *session = (session_t*)handle->data;*/
    /*client_args_t *client_args = CLIENT_ARGS(session);*/

    /*session_free(session);*/
    /*trace_log("on_close() Session id:%d", client_args->session_id);*/
    /*handle->data = NULL;*/
/*}*/

void write_file(session_t *session);
void read_file(session_t *session);
void delete_file(session_t *session);

/* ==================== on_connect() ==================== */ 
static void on_connect(uv_connect_t *req, int status) 
{
    session_t *session = (session_t*)req->handle->data;
    client_t *client = CLIENT(session);

    notice_log("Connected to server %s:%d. op_code:%d", client->ip, client->port, client->op_code);

    if ( client->op_code == MSG_OP_WRITE )
        write_file(session);
    else if ( client->op_code == MSG_OP_READ )
        read_file(session);
    else if ( client->op_code == MSG_OP_DEL )
        delete_file(session);
    else{
        warning_log("Uknown op_code: %d", client->op_code);
    }
}

/* in client_session_handle.c */
void client_session_idle_cb(uv_idle_t *idle_handle, int status);
void client_session_timer_cb(uv_timer_t *timer_handle, int status);
void client_session_async_cb(uv_async_t *async_handle, int status);
int client_session_is_idle(session_t *session);
int client_session_handle_message(session_t *session, message_t *message);
int client_session_init(session_t *session);
void client_session_destroy(session_t *session);

static session_callbacks_t client_callbacks = {
    .idle_cb = client_session_idle_cb,
    .timer_cb = client_session_timer_cb,
    .async_cb = client_session_async_cb,
    .is_idle = client_session_is_idle,
    .handle_message = client_session_handle_message,
    .session_init = client_session_init,
    .session_destroy = client_session_destroy,
    .consume_sockbuf = NULL,
    .on_connect = NULL,
    .handle_read_response = NULL,
};

/* ==================== client_run_task() ==================== */ 
int client_run_task(client_t *client, int session_id)
{
    trace_log("Enter start_connect(). session_id:%d ip=%s, port=%d, op_code=%d", session_id, client->ip, client->port, client->op_code);

    session_callbacks_t *callbacks = &client_callbacks;

    int r;

    /* -------- server_addr -------- */
    struct sockaddr_in server_addr;
    r = uv_ip4_addr(client->ip, client->port, &server_addr);
    if ( r ) {
        error_log("uv_ip4_addr() failed.");
        return -1;
    }

    /* ---------- New session ---------- */

    client_args_t *client_args = zmalloc(sizeof(client_args_t));
    memset(client_args, 0, sizeof(client_args_t));
    client_args->session_id = session_id;
    client_args->op_code = client->op_code;
    client_args->file_data = client->file_data;
    client_args->file_size = client->file_size;
    client_args->file_data_sended = 0;
    /*client_args->file_size = 0;*/
    /*client_args->file = NULL;*/
    client_args->start_index = client->start_index;
    client_args->total_send = 0;
    client_args->total_recv = 0;
    client_args->file_opened = 0;
    client_args->file_closed = 0;

    session_t *session = session_new((void*)client, callbacks, client_args);

    /* -------- loop -------- */
    uv_loop_t *loop = SESSION_LOOP(session);

    /* -------- tcp_handle -------- */
    uv_tcp_t *tcp_handle = SESSION_TCP(session);

    /* -------- uv_tcp_init -------- */
    r = uv_tcp_init(loop, tcp_handle);
    if ( r ) {
        error_log("uv_tcp_init() failed.");
        client_free(client);
        return -1;
    }
    tcp_handle->data = (void*)session; 

    /* -------- uv_tcp_connect -------- */
    r = uv_tcp_connect(&client_args->connect_req,
            tcp_handle,
            (const struct sockaddr*) &server_addr,
            callbacks->on_connect != NULL ? callbacks->on_connect : on_connect);
            /*on_connect);*/
    if ( r ) {
        error_log("uv_tcp_connect() failed.");
        client_free(client);
        return -1;
    }

    /* -------- uv_run -------- */
    r = uv_run(loop, UV_RUN_DEFAULT);
    if ( r ) {
        error_log("uv_run() failed.");
        client_free(client);
        return -1;
    }

    /* FIXME */
    /*MAKE_VALGRIND_HAPPY(loop);*/

    /*close_loop(loop);      */
    /*uv_loop_delete(loop);  */

    notice_log("Session %d exit.", session_id);

    return 0;
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

