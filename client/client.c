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
#include "protocol.h"
#include "legolas.h"
#include "crc32.h"
#include "md5.h"


void write_file(client_t* client);
void read_file(client_t* client);
void delete_file(client_t* client);

void delete_file(client_t* client) {
}

/* ==================== destroy_client() ==================== */ 
void destroy_client(client_t *client){
    assert(client != NULL);
    pthread_mutex_destroy(&client->send_pending_lock);
    pthread_cond_destroy(&client->send_pending_cond);
    pthread_mutex_destroy(&client->recv_pending_lock);
    pthread_cond_destroy(&client->recv_pending_cond);
    zfree(client);
}


/* ==================== on_close() ==================== */ 
void on_close(uv_handle_t* handle) {
    client_t *client = (client_t*)handle->data;
    notice_log("on_close() clientid:%d", client->clientid);

    destroy_client(client);
    handle->data = NULL;
}

/* ==================== on_connect() ==================== */ 
static void on_connect(uv_connect_t *req, int status) {
  client_t *client = (client_t*)req->handle->data;

  notice_log("Connected to server %s:%d. op_code:%d", client->server, client->port, client->op_code);

  if ( client->op_code == MSG_OP_WRITE )
      write_file(client);
  else if ( client->op_code == MSG_OP_READ )
      read_file(client);
  else if ( client->op_code == MSG_OP_DEL )
      delete_file(client);
  else{
      warning_log("Uknown op_code: %d", client->op_code);
  }
}

/* ==================== start_connect() ==================== */ 
int start_connect(int clientid, const char *server, int port, int op_code, const char *key, const char *file, int total_files) 
{
    notice_log("Enter start_connect(). clientid:%d server=%s, port=%d, op_code=%d", clientid, server, port, op_code);

    int r;

    /* -------- loop -------- */
    uv_loop_t client_loop;
    uv_loop_init(&client_loop);
    uv_loop_t *loop = &client_loop;

    trace_log("loop created.");

    /* -------- server_addr -------- */
    struct sockaddr_in server_addr;
    r = uv_ip4_addr(server, port, &server_addr);
    if ( r ) {
        error_log("uv_ip4_addr() failed.");
        return -1;
    }

    /* -------- client -------- */
    client_t *client = (client_t*)zmalloc(sizeof(client_t));
    client->clientid = clientid;
    client->total_send = 0;
    client->total_recv = 0;
    client->server = server;
    client->port = port;
    client->op_code = op_code;
    client->key_prefix = key;
    uint32_t keylen = strlen(key);
    memcpy(client->key, key, keylen);
    client->key[keylen] = '\0';

    client->file = file;
    client->total_files = total_files;
    client->write_request = NULL;


    client->id = 0;
    client->total_readed = 0;
    client->file_size = 0;
    client->connection.total_bytes = 0;

    pthread_mutex_init(&client->send_pending_lock, NULL);
    pthread_cond_init(&client->send_pending_cond, NULL);

    pthread_mutex_init(&client->recv_pending_lock, NULL);
    pthread_cond_init(&client->recv_pending_cond, NULL);

    /* -------- tcp_handle -------- */
    uv_tcp_t *tcp_handle = &client->connection.handle.tcp;

    /* -------- uv_tcp_init -------- */
    r = uv_tcp_init(loop, tcp_handle);
    if ( r ) {
        error_log("uv_tcp_init() failed.");
        destroy_client(client);
        return -1;
    }
    tcp_handle->data = (void*)client; 

    /* -------- uv_tcp_connect -------- */
    r = uv_tcp_connect(&client->connect_req,
            tcp_handle,
            (const struct sockaddr*) &server_addr,
            on_connect);
    if ( r ) {
        error_log("uv_tcp_connect() failed.");
        destroy_client(client);
        return -1;
    }

    /* -------- uv_run -------- */
    r = uv_run(loop, UV_RUN_DEFAULT);
    if ( r ) {
        error_log("uv_run() failed.");
        destroy_client(client);
        return -1;
    }

    /* FIXME */
    /*MAKE_VALGRIND_HAPPY(loop);*/

    /*close_loop(loop);      */
    /*uv_loop_delete(loop);  */

    notice_log("Client %d exit.", clientid);

    return 0;
}

