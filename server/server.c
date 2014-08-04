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
#include "session_handle.h"
#include "list.h"
#include "work.h"
#include "filesystem.h"
#include "storage.h"
#include "common.h"
#include "logfile.h"

int server_init(server_t *server);
int init_server_work_queue(server_t *server);
int exit_server_work_queue(server_t *server);

/* ==================== test_session_consume_sockbuf() ==================== */ 
void test_session_consume_sockbuf(sockbuf_t *sockbuf)
{
    session_t *session = sockbuf->session;

    session_response(session, RESULT_SUCCESS);
    
    sockbuf_free(sockbuf);

    __sync_add_and_fetch(&session->total_saved_buffers, 1);

    pthread_yield();
    /*sched_yield();*/

}

/* ==================== on_connection() ==================== */ 
static void on_connection(uv_stream_t *stream, int status)
{
    server_t *server= (server_t*)stream->data;

    /* -------- create_session -------- */
    static session_callbacks_t callbacks = {
        .idle_cb = session_idle_cb,
        .timer_cb = session_timer_cb,
        .async_cb = session_async_cb,
        .is_idle = session_is_idle,
        .handle_message = session_handle_message,
        .session_init = session_init,
        .session_destroy = session_destroy,
        .consume_sockbuf = NULL,
        /*.consume_sockbuf = test_session_consume_sockbuf*/
        .on_connect = NULL,
        .handle_read_response = NULL,
    };
    session_t *session = session_new((void*)server, &callbacks);
    if ( session == NULL ){
        error_log("session_new() failed. session == NULL.");
        return;
    }
    int ret = session_accept(session, &server->connection.handle.tcp);
    if ( ret != 0 ) {
        error_log("session_accept() failed. ret=%d", ret);
        return;
    }

    info_log("Connected. (server_fd = %d, session_fd = %d)", server_fd(server), session_fd(session));

}

/* ==================== server_new() ==================== */ 
server_t *server_new(void)
{
    server_t *server= (server_t*)zmalloc(sizeof(server_t));
    memset(server, 0, sizeof(server_t));

    server->cached_bytes = 0;

    connection_init(&server->connection);

    return server;
}

/* ==================== server_free() ==================== */ 
void server_free(server_t *server)
{
    /* FIXME */
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

    connection_destroy(&server->connection);

    pthread_mutex_destroy(&server->send_pending_lock);
    pthread_cond_destroy(&server->send_pending_cond);

    zfree(server);
}

/* ==================== start_listen() ==================== */ 
int start_listen(int listen_port, const char *data_dir)
{
    int r;

    /* -------- server_addr -------- */
    struct sockaddr_in server_addr;
    r = uv_ip4_addr("0.0.0.0", listen_port, &server_addr);
    if ( r ) {
        error_log("uv_ip4_addr() failed.");
        return -1;
    }

    /* -------- server-------- */
    server_t *server = server_new();

    if ( server_init(server) != 0 ){
        error_log("server_init() failed.");
        return -1;
    }

    /* -------- loop -------- */
    uv_loop_t *loop = &server->connection.loop;

    /* -------- tcp_handle -------- */
    uv_tcp_t *tcp_handle = &server->connection.handle.tcp;

    /* -------- uv_tcp_init -------- */
    r = uv_tcp_init(loop, tcp_handle);
    if ( r ) {
        error_log("uv_tcp_init() failed.");
        server_free(server);
        return -1;
    }
    tcp_handle->data = server;

    /* -------- uv_tcp_bind -------- */
    r = uv_tcp_bind(tcp_handle, (const struct sockaddr*)&server_addr, 0);
    if ( r ) {
        error_log("uv_tcp_bind() failed.");
        server_free(server);
        return -1;
    }

    /* -------- uv_listen -------- */
    r = uv_listen((uv_stream_t*)tcp_handle, SOMAXCONN, on_connection);
    if ( r ) {
        error_log("uv_listen() failed.");
        server_free(server);
        return -1;
    }

    info_log("Listen on port %d.", listen_port);

    /* -------- uv_run -------- */
    r = uv_run(loop, UV_RUN_DEFAULT);
    if ( r ) {
        error_log("uv_run() failed.");
        server_free(server);
        return -1;
    }

    /* FIXME */
    /*MAKE_VALGRIND_HAPPY(loop);*/

    /*close_loop(loop);      */
    /*uv_loop_delete(loop);  */

    server_free(server);

    notice_log("Server exit.");

    return 0;
}

/* ************************************************************
 *
 *                      Private Functions
 *
 * ************************************************************/

void recv_queue_handle_request(work_queue_t *wq);
void send_queue_handle_response(work_queue_t *wq);

int server_init(server_t *server)
{
    assert(server!= NULL);

	pthread_mutex_init(&server->send_pending_lock, NULL);
	pthread_cond_init(&server->send_pending_cond, NULL);

    UNUSED int r;

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

    /* FIXME */
    r = init_server_work_queue(server);
    if ( r != 0 ){
        error_log("init_server_work_queue() failed.");
        return -1;
    }

    return 0;
}

/*pthread_key_t key_actived_sockbuf;*/

/* ==================== recv_queue_handle_request() ==================== */ 
/**
 * Running in a thread in recv_queue.
 * One thread per session and many sessions per thread.
 */
void session_consume_sockbuf(sockbuf_t *sockbuf); /* in session_sockbuf_message.c */
void recv_queue_handle_request(work_queue_t *wq)
{
    void *nodeData = NULL;
    while ( (nodeData = dequeue_work(wq)) != NULL ){
       session_consume_sockbuf((sockbuf_t*)nodeData);
    }
}

/* ==================== init_server_work_queue() ==================== */ 
int init_server_work_queue(server_t *server)
{
    /*pthread_key_create(&key_actived_sockbuf, NULL);*/

	int i;

	for ( i = 0; i < RECV_THREADS; i++ ) {
		server->recv_queue[i] = init_work_queue(recv_queue_handle_request, RECV_INTERVAL);
		if ( server->recv_queue[i] == NULL ){
			return -1;
        }
	}

    for ( i = 0; i < SEND_THREADS; i++ ) {
        server->send_queue[i] = init_work_queue(send_queue_handle_response, SEND_INTERVAL);
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

    /*pthread_key_delete(key_actived_sockbuf);*/

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

/* ==================== enqueue_recv_queue() ==================== */ 
void enqueue_recv_queue(session_t *session, sockbuf_t *sockbuf)
{
    __sync_add_and_fetch(&session->total_received_buffers, 1);

    work_queue_t *wq = get_recv_queue_by_session(SERVER(session), session);

    if ( wq != NULL ) {
        enqueue_work(wq, (void*)sockbuf);
    }
}

/* ==================== dequeue_recv_queue() ==================== */ 
sockbuf_t *dequeue_recv_queue(session_t *session)
{
    work_queue_t *wq = get_recv_queue_by_session(SERVER(session), session);

    if ( wq != NULL ) {
        return dequeue_work(wq);
    }

    return NULL;
}
