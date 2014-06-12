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
#include "lockfree_queue.h"

void empty_server_info(server_info_t *server_info);
int server_init(server_info_t *server_info);
int init_server_work_queue(server_info_t *server_info);
int exit_server_work_queue(server_info_t *server_info);


/* ==================== on_connection() ==================== */ 
static void on_connection(uv_stream_t *stream, int status)
{
    server_info_t *server_info = (server_info_t*)stream->data;

    /* -------- create_session -------- */
    session_info_t *session_info = create_session(server_info);
    if ( session_info == NULL ){
        error_log("create_session() failed. session_info == NULL.");
        return;
    }

    info_log("Connected. (server_fd = %d, session_fd = %d)", server_fd(server_info), session_fd(session_info));

}

/* ==================== server_destroy() ==================== */ 
void server_destroy(server_info_t *server_info)
{
    exit_server_work_queue(server_info);

    int i;
    for ( i = 0 ; i < MAX_VNODES ; i++ ){
        vnode_info_t *vnode = server_info->vnodes[i];
        vnode_destroy(vnode);
    }

    zfree(server_info);
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

    /* -------- server_info -------- */
    server_info_t *server_info = (server_info_t*)zmalloc(sizeof(server_info_t));
    empty_server_info(server_info);

    if ( server_init(server_info) != 0 ){
        error_log("server_init() failed.");
        return -1;
    }

    /* -------- tcp_handle -------- */
    uv_tcp_t *tcp_handle = &server_info->tcp_handle;

    /* -------- uv_tcp_init -------- */
    r = uv_tcp_init(loop, tcp_handle);
    if ( r ) {
        error_log("uv_tcp_init() failed.");
        server_destroy(server_info);
        return -1;
    }
    tcp_handle->data = server_info;

    /* -------- uv_tcp_bind -------- */
    r = uv_tcp_bind(tcp_handle, (const struct sockaddr*)&server_addr, 0);
    if ( r ) {
        error_log("uv_tcp_bind() failed.");
        server_destroy(server_info);
        return -1;
    }

    /* -------- uv_listen -------- */
    r = uv_listen((uv_stream_t*)tcp_handle, SOMAXCONN, on_connection);
    if ( r ) {
        error_log("uv_listen() failed.");
        server_destroy(server_info);
        return -1;
    }

    info_log("Listen on port %d.", listen_port);

    /* -------- uv_run -------- */
    r = uv_run(loop, UV_RUN_DEFAULT);
    if ( r ) {
        error_log("uv_run() failed.");
        server_destroy(server_info);
        return -1;
    }

    /* FIXME */
    /*MAKE_VALGRIND_HAPPY(loop);*/

    /*close_loop(loop);      */
    /*uv_loop_delete(loop);  */

    server_destroy(server_info);

    notice_log("Server exit.");

    return 0;
}

/* ************************************************************
 *
 *                      Private Functions
 *
 * ************************************************************/

void recv_request(struct work_queue *wq);
void send_response(struct work_queue *wq);

void empty_server_info(server_info_t *server_info)
{
    memset(server_info, 0, sizeof(server_info_t));
    server_info->cached_bytes = 0;
}

int server_init(server_info_t *server_info)
{
    assert(server_info != NULL);

    int r;

    get_instance_parent_full_path(server_info->root_dir, NAME_MAX);
    sprintf(server_info->storage_info.storage_dir, "%s/data/storage", server_info->root_dir);
    if ( mkdir_if_not_exist(server_info->storage_info.storage_dir) != 0 ){
        error_log("mkdir %s failed.", server_info->storage_info.storage_dir);
        return -1;
    }

    /*r = storage_init(&server_info->storage_info);*/
    int i;
    for ( i = 0 ; i < MAX_VNODES ; i++ ){
        server_info->vnodes[i] = (vnode_info_t*)zmalloc(sizeof(vnode_info_t));
        r = vnode_init(server_info->vnodes[i], server_info->storage_info.storage_dir, i);
        if ( r != 0 ){
            error_log("vnode_init() failed. id:%d", i);
            return -1;
        }
    }

    r = init_server_work_queue(server_info);
    if ( r != 0 ){
        error_log("init_server_work_queue() failed.");
        return -1;
    }

    return 0;
}

/*pthread_key_t key_actived_cob;*/

/* ==================== init_server_work_queue() ==================== */ 
int init_server_work_queue(server_info_t *server_info)
{
    /*pthread_key_create(&key_actived_cob, NULL);*/

	int i;

	for ( i = 0; i < RECV_THREADS; i++ ) {
		server_info->recv_queue[i] = init_work_queue(recv_request, RECV_INTERVAL);
		if ( server_info->recv_queue[i] == NULL ){
			return -1;
        }
	}

	/*for ( i = 0; i < SEND_THREADS; i++ ) {*/
		/*server_info->send_queue[i] = init_work_queue(send_response, SEND_INTERVAL);*/
		/*if ( server_info->send_queue[i] == NULL )*/
			/*return -1;*/
	/*}*/

    return 0;
}

/* ==================== exit_server_work_queue() ==================== */ 
int exit_server_work_queue(server_info_t *server_info)
{
	int i;

	for ( i = 0; i < RECV_THREADS; i++ ) {
        struct work_queue *wq = server_info->recv_queue[i];
        if ( wq != NULL ) {
            exit_work_queue(wq);
            zfree(wq);
            server_info->recv_queue[i] = NULL;
        }
	}

	/*for ( i = 0; i < SEND_THREADS; i++ ) {*/
        /*struct work_queue *wq = server_info->send_queue[i];*/
        /*if ( wq != NULL ) {*/
            /*exit_work_queue(wq);*/
            /*zfree(wq);*/
            /*server_info->send_queue[i] = NULL;*/
        /*}*/
	/*}*/

    /*pthread_key_delete(key_actived_cob);*/

    return 0;
}

