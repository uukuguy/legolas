/**
 * @file   server.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-04-30 22:06:25
 * 
 * @brief  
 * 
 * 
 */

#include "legolas.h"
#include "server.h"
#include "vnode.h"
#include "session.h"
#include "list.h"
#include "work.h"
#include "filesystem.h"
#include "storage.h"
#include "common.h"
#include "logfile.h"

void empty_server(server_t *server);
int server_init(server_t *server);
int init_server_work_queue(server_t *server);
int exit_server_work_queue(server_t *server);


/* ==================== on_connection() ==================== */ 
static void on_connection(uv_stream_t *stream, int status)
{
    server_t *server= (server_t*)stream->data;

    /* -------- create_session -------- */
    session_t *session = create_session(server);
    if ( session == NULL ){
        error_log("create_session() failed. session == NULL.");
        return;
    }

    info_log("Connected. (server_fd = %d, session_fd = %d)", server_fd(server), session_fd(session));

}

/* ==================== server_destroy() ==================== */ 
void server_destroy(server_t *server)
{
    exit_server_work_queue(server);

    int i;
    for ( i = 0 ; i < VNODES ; i++ ){
        vnode_t *vnode = server->vnodes[i];
        if ( vnode != NULL ){
            vnode_free(vnode);
            server->vnodes[i] = NULL;
        }
    }

    for ( i = 0 ; i < LOGFILES ; i++ ) {
        logfile_t *logfile = server->logfiles[i];
        if ( logfile != NULL ) {
            logfile_close(logfile);
            logfile_free(logfile);
            server->logfiles[i] = NULL;
        }
    }

    zfree(server);
}

/* ==================== start_listen() ==================== */ 
int start_listen(int listen_port, const char *data_dir)
{
    int r;

    /* -------- loop -------- */
    uv_loop_t session_loop;
    uv_loop_init(&session_loop);
    uv_loop_t *loop = &session_loop;

    /* -------- server_addr -------- */
    struct sockaddr_in server_addr;
    r = uv_ip4_addr("0.0.0.0", listen_port, &server_addr);
    if ( r ) {
        error_log("uv_ip4_addr() failed.");
        return -1;
    }

    /* -------- server-------- */
    server_t *server= (server_t*)zmalloc(sizeof(server_t));
    empty_server(server);

    if ( server_init(server) != 0 ){
        error_log("server_init() failed.");
        return -1;
    }

    /* -------- tcp_handle -------- */
    uv_tcp_t *tcp_handle = &server->tcp_handle;

    /* -------- uv_tcp_init -------- */
    r = uv_tcp_init(loop, tcp_handle);
    if ( r ) {
        error_log("uv_tcp_init() failed.");
        server_destroy(server);
        return -1;
    }
    tcp_handle->data = server;

    /* -------- uv_tcp_bind -------- */
    r = uv_tcp_bind(tcp_handle, (const struct sockaddr*)&server_addr, 0);
    if ( r ) {
        error_log("uv_tcp_bind() failed.");
        server_destroy(server);
        return -1;
    }

    /* -------- uv_listen -------- */
    r = uv_listen((uv_stream_t*)tcp_handle, SOMAXCONN, on_connection);
    if ( r ) {
        error_log("uv_listen() failed.");
        server_destroy(server);
        return -1;
    }

    info_log("Listen on port %d.", listen_port);

    /* -------- uv_run -------- */
    r = uv_run(loop, UV_RUN_DEFAULT);
    if ( r ) {
        error_log("uv_run() failed.");
        server_destroy(server);
        return -1;
    }

    /* FIXME */
    /*MAKE_VALGRIND_HAPPY(loop);*/

    /*close_loop(loop);      */
    /*uv_loop_delete(loop);  */

    server_destroy(server);

    notice_log("Server exit.");

    return 0;
}

/* ************************************************************
 *
 *                      Private Functions
 *
 * ************************************************************/

void recv_request(work_queue_t *wq);
void send_response(work_queue_t *wq);

void empty_server(server_t *server)
{
    memset(server, 0, sizeof(server_t));
    server->cached_bytes = 0;
}

int server_init(server_t *server)
{
    assert(server!= NULL);

    int r;

    get_instance_parent_full_path(server->root_dir, NAME_MAX);

    sprintf(server->storage.storage_dir, "%s/data/storage", server->root_dir);
    if ( mkdir_if_not_exist(server->storage.storage_dir) != 0 ){
        error_log("mkdir %s failed.", server->storage.storage_dir);
        return -1;
    }

    char log_dir[NAME_MAX];
    sprintf(log_dir, "%s/data/log", server->root_dir);
    if ( mkdir_if_not_exist(log_dir) != 0 ) {
        error_log("mkdir %s failed.", log_dir);
        return -1;
    }

    /*r = storage_init(&server->storage);*/
    int i;
    for ( i = 0 ; i < VNODES ; i++ ){
        vnode_t *vnode = vnode_new(server->storage.storage_dir, i);
        if ( vnode == NULL ){
            error_log("vnode_init() failed. id:%d", i);
            return -1;
        }
        server->vnodes[i] = vnode;
    }

    /* logfile */
    for ( i = 0 ; i < LOGFILES ; i++ ) {
        char logfile_name[NAME_MAX];
        sprintf(logfile_name, "%s/%02d.log", log_dir, i);

        logfile_t *logfile = logfile_new(i, logfile_name);

        if ( logfile_open(logfile, 1) != 0 ) {
            error_log("logfile_open(%d) failed.", i);
            return -1;
        }

        server->logfiles[i] = logfile;
    }

    r = init_server_work_queue(server);
    if ( r != 0 ){
        error_log("init_server_work_queue() failed.");
        return -1;
    }

    return 0;
}

/*pthread_key_t key_actived_cob;*/

/* ==================== init_server_work_queue() ==================== */ 
int init_server_work_queue(server_t *server)
{
    /*pthread_key_create(&key_actived_cob, NULL);*/

	int i;

	for ( i = 0; i < RECV_THREADS; i++ ) {
		server->recv_queue[i] = init_work_queue(recv_request, RECV_INTERVAL);
		if ( server->recv_queue[i] == NULL ){
			return -1;
        }
	}

    for ( i = 0; i < SEND_THREADS; i++ ) {
        server->send_queue[i] = init_work_queue(send_response, SEND_INTERVAL);
        if ( server->send_queue[i] == NULL )
            return -1;
    }

    return 0;
}

/* ==================== exit_server_work_queue() ==================== */ 
int exit_server_work_queue(server_t *server)
{
	int i;

	for ( i = 0; i < RECV_THREADS; i++ ) {
        work_queue_t *wq = server->recv_queue[i];
        if ( wq != NULL ) {
            exit_work_queue(wq);
            zfree(wq);
            server->recv_queue[i] = NULL;
        }
	}

    for ( i = 0; i < SEND_THREADS; i++ ) {
        work_queue_t *wq = server->send_queue[i];
        if ( wq != NULL ) {
            exit_work_queue(wq);
            zfree(wq);
            server->send_queue[i] = NULL;
        }
    }

    /*pthread_key_delete(key_actived_cob);*/

    return 0;
}


vnode_t *get_vnode_by_key(server_t *server, md5_value_t *key_md5)
{
    vnode_t *vnode = NULL;

    int d1 = key_md5->h1 % VNODES;
    vnode = server->vnodes[d1];

    return vnode;
}

logfile_t *get_logfile_by_session(server_t *server, session_t *session)
{
    int fd = session_fd(session);    
    int idx = fd % LOGFILES;
    assert(idx >= 0 && idx < LOGFILES);
    return server->logfiles[idx];
}

work_queue_t *get_recv_queue_by_session(server_t *server, session_t *session)
{

    int fd = session_fd(session);    
    int idx = fd % RECV_THREADS;
    assert(idx >= 0 && idx < RECV_THREADS);
    return server->recv_queue[idx];
}

work_queue_t *get_send_queue_by_session(server_t *server, session_t *session)
{

    int fd = session_fd(session);    
    int idx = fd % SEND_THREADS;
    assert(idx >= 0 && idx < SEND_THREADS);
    return server->send_queue[idx];
}

