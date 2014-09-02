/**
 * @file   udclient.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-09-09 13:29:43
 * 
 * @brief  
 * 
 * 
 */

#include "udclient.h"
#include "session.h"
#include "legolas.h"

#include "zmalloc.h"
#include "list.h"
#include "adlist.h"
#include "uv.h"
#include "logger.h"

/* ==================== udclient_new() ==================== */ 
udclient_t *udclient_new(void)
{
    udclient_t *udcli = (udclient_t*)zmalloc(sizeof(udclient_t));
    memset(udcli, 0, sizeof(udclient_t));

    return udcli;
}


void udclient_free(udclient_t *udcli)
{
    if ( udcli != NULL ) {
        zfree(udcli);
    }
}

/* ==================== udclient_open_data() ==================== */ 
int udclient_open_data(udclient_t *udcli, const char *key)
{
    return -1;
}

/* ==================== udclient_write_data() ==================== */ 
int udclient_write_data(udclient_t *udcli, int handle, void *data, uint32_t len)
{
    return -1;
}

/* ==================== udclient_write_data() ==================== */ 
int udclient_read_data(udclient_t *udcli, int handle, void *data, uint32_t len)
{
    return -1;
}

/* ==================== udclient_close_data() ==================== */ 
int udclient_close_data(udclient_t *udcli, int handle)
{
    return -1;
}

/* ==================== udclient_delete_data() ==================== */ 
int udclient_delete_data(udclient_t *udcli, const char *key)
{
    return 0;
}

/* ==================== on_connect() ==================== */ 
static void on_connect(uv_connect_t *req, int status) 
{
    session_t *session = (session_t*)req->handle->data;
    UNUSED udclient_t *udcli= UDCLIENT(session);

    /*notice_log("Connected to server %s:%d. op_code:%d", client->ip, client->port, client->op_code);*/

    /*if ( client->op_code == MSG_OP_WRITE )*/
        /*write_file(session);*/
    /*else if ( client->op_code == MSG_OP_READ )*/
        /*read_file(session);*/
    /*else if ( client->op_code == MSG_OP_DEL )*/
        /*delete_file(session);*/
    /*else{*/
        /*warning_log("Uknown op_code: %d", client->op_code);*/
    /*}*/
}

/* ==================== udclient_create_session() ==================== */ 
static int udclient_create_session(udclient_t *udcli)
{
    int r;

    /* FIXME */
    const char *ip = "127.0.0.1";
    int port = DEFAULT_PORT;
    session_callbacks_t *callbacks = NULL;

    /* -------- server_addr -------- */
    struct sockaddr_in server_addr;
    r = uv_ip4_addr(ip, port, &server_addr);
    if ( r ) {
        error_log("uv_ip4_addr() failed.");
        return -1;
    }

    /* ---------- New session ---------- */
    session_t *session = session_new((void*)udcli, callbacks, NULL);

    /* -------- loop -------- */
    uv_loop_t *loop = SESSION_LOOP(session);

    /* -------- tcp_handle -------- */
    uv_tcp_t *tcp_handle = SESSION_TCP(session);

    /* -------- uv_tcp_init -------- */
    r = uv_tcp_init(loop, tcp_handle);
    if ( r ) {
        error_log("uv_tcp_init() failed.");
        udclient_free(udcli);
        return -1;
    }
    tcp_handle->data = (void*)session; 

    /* -------- uv_tcp_connect -------- */
    uv_connect_t connect_req;
    r = uv_tcp_connect(&connect_req,
            tcp_handle,
            (const struct sockaddr*) &server_addr,
            callbacks != NULL && callbacks->on_connect != NULL ? callbacks->on_connect : on_connect);
            /*on_connect);*/
    if ( r ) {
        error_log("uv_tcp_connect() failed.");
        udclient_free(udcli);
        return -1;
    }

    /* -------- uv_run -------- */
    r = uv_run(loop, UV_RUN_DEFAULT);
    if ( r ) {
        error_log("uv_run() failed.");
        udclient_free(udcli);
        return -1;
    }

    /* FIXME */
    /*MAKE_VALGRIND_HAPPY(loop);*/

    /*close_loop(loop);      */
    /*uv_loop_delete(loop);  */

    return 0;
}

/* ==================== udclient_thread_main() ==================== */ 
static void* udclient_thread_main(void *arg)
{
    udclient_t *udcli = (udclient_t*)arg;

    trace_log("Enter udclient_thread_main().");

    int rc = udclient_create_session(udcli);
    if ( rc != 0 ){
        error_log("udclient_creat_session() failed.");
    }

    notice_log("udclient_thread_main() %d exit.", udcli->id);

    return NULL;
}

/* ==================== udclient_run() ==================== */ 
int udclient_run(udclient_t *udcli)
{
    int rc = 0;

    /*pthread_attr_t attr;*/
    /*pthread_attr_init(&attr);*/
    /*phread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);*/
    rc = pthread_create(&udcli->tid, NULL, udclient_thread_main, udcli);

    return rc;
}

/* ==================== udclient_exit() ==================== */ 
void udclient_exit(udclient_t *udcli)
{
    pthread_cancel(udcli->tid);
}

