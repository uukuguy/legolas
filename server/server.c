/**
 * @file   server.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-04-30 22:06:25
 * 
 * @brief  
 * 
 * 
 */

#include "server.h"
#include "service.h"
#include "session.h"
#include "vnode.h"
#include "server_handle.h"
#include "list.h"
#include "work.h"
#include "filesystem.h"
#include "storage.h"
#include "common.h"
#include "logfile.h"
#include "sysinfo.h"

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

/* ==================== server_on_connection() ==================== */ 
static void server_on_connection(service_t *service, int status)
{
    server_t *server = (server_t*)service->parent;

    /* -------- create_session -------- */
    session_t *session = session_new(server->service, NULL);
    if ( session == NULL ){
        error_log("session_new() failed. session == NULL.");
        return;
    }

    int ret = session_accept(session, &service->connection.handle.tcp);
    if ( ret != 0 ) {
        error_log("session_accept() failed. ret=%d", ret);
        return;
    }

    info_log("Connected. (server_fd = %d, session_fd = %d)", server_fd(server), session_fd(session));

}

/* ==================== server_new() ==================== */ 
server_t *server_new(const server_options_t *server_options)
{
    server_t *server= (server_t*)zmalloc(sizeof(server_t));
    memset(server, 0, sizeof(server_t));
    memcpy(&server->options, server_options, sizeof(server_options_t));

    server->cached_bytes = 0;
    server->total_requests = 0;

    sysinfo_t sysinfo;
    flush_sysinfo(&sysinfo);
    server->num_processors = sysinfo.num_processors;

    static session_callbacks_t callbacks = {
        .idle_cb = server_idle_cb,
        .timer_cb = session_timer_cb,
        .async_cb = session_async_cb,
        .is_idle = server_is_idle,
        .handle_message = server_handle_message,
        .session_init = session_init,
        .session_destroy = session_destroy,
        .consume_sockbuf = NULL,
        /*.consume_sockbuf = test_session_consume_sockbuf*/
        .on_connect = NULL,
        .handle_read_response = NULL,

        .session_on_connect_from_client = server_on_connection,
    };
    server->service = service_new(server, &callbacks);


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

    service_destroy(server->service);

    pthread_mutex_destroy(&server->send_pending_lock);
    pthread_cond_destroy(&server->send_pending_cond);

    zfree(server);
}

/* ==================== server_listen() ==================== */ 
int server_listen(server_t *server) 
{
    int r;
    int listen_port = server->options.listen_port;

    r = session_listen(server->service, listen_port);

    return r;
}

/* ************************************************************
 *
 *                      Private Functions
 *
 * ************************************************************/

void recv_queue_handle_request(work_queue_t *wq);
void send_queue_handle_response(work_queue_t *wq);
void vnode_write_queue_handle_write(work_queue_t *wq); // in server_handle_write.c
/* ==================== server_create_vnode() ==================== */ 
vnode_t * server_create_vnode(server_t *server, int vnode_id)
{
    /* -------- storage_dir -------- */
    char storage_dir[NAME_MAX];
    sprintf(storage_dir, "%s/storage", server->data_dir);
    if ( mkdir_if_not_exist(storage_dir) != 0 ){
        error_log("mkdir %s failed.", storage_dir);
        return NULL;
    }

    vnode_t *vnode = vnode_new(storage_dir, vnode_id, server->options.storage_type, vnode_write_queue_handle_write);
    server->vnodes[vnode_id] = vnode;

    return vnode;
}

/* ==================== server_init() ==================== */ 
int server_init(server_t *server)
{
    assert(server!= NULL);

	pthread_mutex_init(&server->send_pending_lock, NULL);
	pthread_cond_init(&server->send_pending_cond, NULL);

    UNUSED int r;

    /* -------- server->root_dir -------- */
    get_instance_parent_full_path(server->root_dir, NAME_MAX);

    /* -------- server->data_dir -------- */
    if ( server->options.data_dir == NULL ){
        sprintf(server->data_dir, "%s/data", server->root_dir);
    } else if ( server->options.data_dir[0] == '/' ) {
        sprintf(server->data_dir, "%s", server->options.data_dir);
    } else {
        sprintf(server->data_dir, "%s/%s", server->root_dir, server->options.data_dir);
    }

    /* -------- Create vnodes -------- */
    int i;
    for ( i = 0 ; i < VNODES ; i++ ){
        vnode_t *vnode = server_create_vnode(server, i);
        if ( vnode == NULL ){
            error_log("vnode_init() failed. id:%d", i);
            return -1;
        }
    }

    /* -------- logfile_dir -------- */
    char logfile_dir[NAME_MAX];
    sprintf(logfile_dir, "%s/log", server->data_dir);
    if ( mkdir_if_not_exist(logfile_dir) != 0 ) {
        error_log("mkdir %s failed.", logfile_dir);
        return -1;
    }

    for ( i = 0 ; i < LOGFILES ; i++ ) {
        char logfile_name[NAME_MAX];
        sprintf(logfile_name, "%s/%02d.log", logfile_dir, i);

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

    /* -------- recv_queue -------- */
    uint32_t recv_threads = server->num_processors;
    server->recv_threads = recv_threads;

    server->recv_queue = (work_queue_t**)zmalloc(sizeof(work_queue_t*) * recv_threads);

	for ( i = 0; i < recv_threads; i++ ) {
		server->recv_queue[i] = init_work_queue(recv_queue_handle_request, RECV_INTERVAL);
		if ( server->recv_queue[i] == NULL ){
			return -1;
        }
	}

    /* -------- send_queue -------- */
    uint32_t send_threads = server->num_processors;
    server->send_threads = send_threads;

    server->send_queue = (work_queue_t**)zmalloc(sizeof(work_queue_t*) * send_threads);

    for ( i = 0; i < send_threads; i++ ) {
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

    /* -------- recv_queue -------- */
    uint32_t recv_threads = server->recv_threads;
	for ( i = 0; i < recv_threads; i++ ) {
        work_queue_t *wq = server->recv_queue[i];
        if ( wq != NULL ) {
            exit_work_queue(wq);
            zfree(wq);
            server->recv_queue[i] = NULL;
        }
	}
    zfree(server->recv_queue);
    server->recv_queue = NULL;

    /* -------- send_queue -------- */
    uint32_t send_threads = server->send_threads;
    for ( i = 0; i < send_threads; i++ ) {
        work_queue_t *wq = server->send_queue[i];
        if ( wq != NULL ) {
            exit_work_queue(wq);
            zfree(wq);
            server->send_queue[i] = NULL;
        }
    }
    zfree(server->send_queue);
    server->send_queue = NULL;

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

    uint32_t recv_threads = server->recv_threads;

    int fd = session_fd(session);    
    int idx = fd % recv_threads;
    assert(idx >= 0 && idx < recv_threads);
    return server->recv_queue[idx];
}

work_queue_t *get_send_queue_by_session(server_t *server, session_t *session)
{
    uint32_t send_threads = server->send_threads;

    int fd = session_fd(session);    
    int idx = fd % send_threads;
    assert(idx >= 0 && idx < send_threads);
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

