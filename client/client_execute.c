/**
 * @file   client_execute.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-08-08 15:40:01
 * 
 * @brief  
 * 
 * 
 */

#include "uv.h"
#include "common.h"
#include "work.h"
#include "filesystem.h"
#include "logger.h"
#include "message.h"
#include "client.h"
#include "session.h"
#include "crc32.h"
#include "md5.h"

/* in client_read.c */
extern int do_read_request(session_t *session);
extern int handle_read_response(session_t *session, message_t *response);

int process_read_response_data(session_t *session, msgidx_t *msgidx)
{
    client_t *client = CLIENT(session);
    message_t *response = msgidx->message;

    if ( response->result == RESULT_SUCCESS ) {
        const char *data = msgidx->data;
        uint32_t data_size = msgidx->data_size;

        int file = open(client->filename, O_CREAT | O_TRUNC | O_WRONLY, 0640);
        write(file, data, data_size);
        close(file);
    }

    return 0;
}

/*  ==================== execute_handle_read_response() ==================== */ 
int execute_handle_read_response(session_t *session, message_t *response)
{

    UNUSED client_t *client = CLIENT(session);
    UNUSED client_args_t *client_args = CLIENT_ARGS(session);

    int ret;
    ret = handle_read_response(session, response);

    session->waiting_for_close = 1;
    session_shutdown(session);

    return ret;
}

int execute_handle_message(session_t *session, message_t *message)
{
    int ret = 0;
    client_args_t *client_args = CLIENT_ARGS(session);

    switch ( client_args->op_code ){
        case MSG_OP_WRITE:
            {
                if ( message->msg_type == MSG_TYPE_REQUEST ){
                } else if ( message->msg_type == MSG_TYPE_RESPONSE ) {
                }
            } break;
        case MSG_OP_READ:
            {
                if ( message->msg_type == MSG_TYPE_REQUEST ){
                } else if ( message->msg_type == MSG_TYPE_RESPONSE ) {
                    ret = execute_handle_read_response(session, message);
                }
            } break;
        case MSG_OP_DEL:
            {
                if ( message->msg_type == MSG_TYPE_REQUEST ){
                } else if ( message->msg_type == MSG_TYPE_RESPONSE ) {
                }
            } break;
    }


    return ret;
}

/* ==================== execute_read_file() ==================== */ 
void execute_read_file(session_t* session) 
{
    client_t *client = CLIENT(session);
    client_args_t *client_args = CLIENT_ARGS(session);

    session->connection.total_bytes = 0;

    /*char file_name[NAME_MAX];*/
    /*get_path_file_name(client->filename, file_name, NAME_MAX - 1);*/

    /*sprintf(client_args->key, "/test/%s/%04d/%08d-%s", client->key_prefix, client_args->session_id, client_args->total_recv, file_name);*/

    sprintf(client_args->key, "%s", client->key_prefix);
    trace_log("read_file(): %s", client_args->key);

    do_read_request(session);
}

/* ==================== execute_write_file() ==================== */ 
void execute_write_file(session_t* session) 
{
    UNUSED client_t *client = CLIENT(session);
    UNUSED client_args_t *client_args = CLIENT_ARGS(session);
}

/* ==================== execute_delete_file() ==================== */ 
void execute_delete_file(session_t* session) 
{
    UNUSED client_t *client = CLIENT(session);
    UNUSED client_args_t *client_args = CLIENT_ARGS(session);
}

/* ==================== client_execute_on_connect() ==================== */ 
void execute_on_connect(uv_connect_t *req, int status) 
{
    session_t *session = (session_t*)req->handle->data;
    client_t *client = CLIENT(session);

    notice_log("Connected to server %s:%d. op_code:%d", client->ip, client->port, client->op_code);

    if ( client->op_code == MSG_OP_WRITE )
        execute_write_file(session);
    else if ( client->op_code == MSG_OP_READ )
        execute_read_file(session);
    else if ( client->op_code == MSG_OP_DEL )
        execute_delete_file(session);
    else{
        warning_log("Uknown op_code: %d", client->op_code);
    }
}

/*static session_callbacks_t session_callbacks = {*/
    /*.idle_cb = NULL,*/
    /*.timer_cb = NULL,*/
    /*.async_cb = NULL,*/
    /*.is_idle = NULL,*/
    /*.handle_message = execute_handle_message,*/
    /*.session_init = NULL,*/
    /*.session_destroy = NULL,*/
    /*.consume_sockbuf = NULL,*/
    /*.on_connect = execute_on_connect,*/
    /*.handle_read_response = process_read_response_data,*/
/*};*/

/* ==================== client_execute() ==================== */ 
int client_execute(client_t *client)
{
    /*return start_connect(client, &session_callbacks, 0);*/
    return -1;

}

