#ifndef __LEGOLAS_H__
#define __LEGOLAS_H__

#include "zmalloc.h"
#include "list.h"
#include "uv.h"
#include "work.h"
#include "logger.h"
#include "coroutine.h"
#include "protocol.h"
//#include "../server/vnode.h"
#include <time.h>

#define DEFAULT_PORT 16076

#define DEFAULT_CONN_BUF_SIZE 64 * 1024

struct server_info_t;
struct session_info_t;
struct storage_file_t;

typedef struct conn_buf_t {
    char base[DEFAULT_CONN_BUF_SIZE + 1024];
    uint32_t read_head;
    uint32_t read_eob;
    uint32_t read_tail;
    uint32_t write_head;
    uint32_t write_tail;
    uint32_t len;
    uint32_t remain_bytes;
    struct session_info_t *session_info;

    struct list_head rx_block_list;

    uint32_t blockid;
    uint32_t least_size;

} conn_buf_t;

typedef struct conn_t {
  struct session_info_t *session_info;  /* Backlink to owning client context. */
  union {
    uv_handle_t handle;
    uv_stream_t stream;
    uv_tcp_t tcp;
    uv_udp_t udp;
  } handle;

  conn_buf_t cob;
  uint32_t total_bytes;

  unsigned char rdstate;
  unsigned char wrstate;
  unsigned int idle_timeout;

  ssize_t result;
  uv_timer_t timer_handle;  /* For detecting timeouts. */
  uv_write_t write_req;
  /* We only need one of these at a time so make them share memory. */
  union {
    uv_getaddrinfo_t addrinfo_req;
    uv_connect_t connect_req;
    uv_req_t req;
    struct sockaddr_in6 addr6;
    struct sockaddr_in addr4;
    struct sockaddr addr;
  } t;
} conn_t;

typedef struct session_id {
	union {
		struct {
			uint32_t nodeid;
			uint32_t seq_no;
		};
		uint64_t id;
	};
	struct list_head id_list;
} session_id;

enum session_status {
	SESSION_STATUS_HEAD,
	SESSION_STATUS_BODY,
	SESSION_STATUS_END,
};

struct co_buffer {
	size_t offset;
	size_t len;
	char *buf;
};

/* struct coroutine; */

typedef struct session_info_t{
    struct session_id sid;
    enum session_status session_status;
    struct server_info_t *server_info;  /* Backlink to owning server context. */
    conn_t connection;  /* Connection with the SOCKS client. */

    uint32_t total_received_buffers;
    uint32_t total_saved_buffers;

    struct list_head rx_list;
    int stop; /* if true, the connection is not ready for read
               * operations because of too many requests */

    uv_idle_t idle_handle;
    uv_async_t async_handle;
    uint32_t finished_works;
    
    uint32_t file_size;
    char key[NAME_MAX];

    struct coroutine *rx_co;
    struct coroutine *tx_co; 

    int refcnt;
    int waiting_for_close;

    pthread_mutex_t recv_pending_lock;
    pthread_cond_t recv_pending_cond;

    pthread_mutex_t send_pending_lock;
    pthread_cond_t send_pending_cond;

    pthread_mutex_t after_response_lock;
    pthread_cond_t after_response_cond;

    int is_sending;

    /* FILE* f; */
    struct storage_file_t *f;

    uint32_t total_writed;

    struct list_head rx_block_queue;

    uint32_t total_blocks;
    uint32_t cached_bytes;

    /* .................................. */

    unsigned int state;
    struct co_buffer rx_buf;

    struct list_head siblings;

    struct list_head tx_list;
    int tx_on; /* if true, send_response() is sending response through
                * this connection */
    pthread_mutex_t tx_lock; /* protect tx_on and rsp_list */

    struct list_head rsp_list;

    int tx_failed;


    int outstanding_reqs;

} session_info_t;

#define session_handle(session_info) \
    session_info->connection.handle.tcp

#define session_stream(session_info) \
    session_info->connection.handle.stream

#define session_tcp(session_info) \
    session_info->connection.handle.tcp

#define session_udp(session_info) \
    session_info->connection.handle.udp

typedef struct response_t{
	/* struct mtrd_req *rq;
	struct mtrd_common_hdr *msg;*/

	struct session_info_t *session_info;

	struct list_head siblings;

	struct list_head w_list;
} response_t;

typedef struct client_info_t {
    uv_connect_t connect_req;
    conn_t connection;
    const char *server;
    int port;
    const char *key;
    const char *file;

    pthread_mutex_t send_pending_lock;
    pthread_cond_t send_pending_cond;

    uint32_t id; /* block id */
    uint32_t total_readed;
    uint32_t file_size;
    uint32_t total_files;
    FILE* f;
    int clientid;
    int total_send;

    void *write_request;

} client_info_t;

/* Get file descripter in uv_stream_t. */
#define stream_fd(stream) \
    stream->io_watcher.fd

/* Get file descripter in server_info_t. */
#define server_fd(server_info) \
    server_info->tcp_handle.io_watcher.fd

/* Get file descripter in session_info_t. */
#define session_fd(session_info) \
    session_info->connection.handle.stream.io_watcher.fd

/* Get file descripter in session_info_t. */
#define client_fd(client_info) \
    client_info->connect_req.handle->io_watcher.fd

/* Get file descripter in session_info_t. */
#define client_fd1(client_info) \
    client_info->connection.handle.stream.io_watcher.fd

/* Fully close a loop */

static void close_walk_cb(uv_handle_t* handle, void* arg) {
  if (!uv_is_closing(handle))
    uv_close(handle, NULL);
}

UNUSED static void close_loop(uv_loop_t* loop) {
  uv_walk(loop, close_walk_cb, NULL);
  uv_run(loop, UV_RUN_DEFAULT);
}

/* This macro cleans up the main loop. This is used to avoid valgrind
 * warnings about memory being "leaked" by the main event loop.
 */
#define MAKE_VALGRIND_HAPPY(loop)           \
  do {                                  \
    close_loop(loop);      \
    uv_loop_delete(loop);  \
  } while (0)

#endif /* __LEGOLAS_H__ */


