/**
 * @file   session.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-07-07 01:49:33
 * 
 * @brief  
 * 
 * 
 */

#ifndef __SESSION_H__
#define __SESSION_H__

#include "zmalloc.h"
#include "list.h"
#include "adlist.h"
#include "uv.h"
#include "work.h"
#include "logger.h"
#include "message.h"
#include "coro.h"
#include <time.h>

//typedef struct schedule schedule;

#define MAX_CACHED_BYTES 200 * 1024 * 1024

#define DEFAULT_SOCKBUF_SIZE 64 * 1024

typedef struct network_instance_t network_instance_t;
typedef struct service_t service_t;
typedef struct session_t session_t;
typedef struct storage_file_t storage_file_t;
typedef struct object_t object_t;

#define USE_CGREENLET
//#define USE_LIBCORO
//#define USE_COROUTINE

//typedef struct coroutine_t coroutine_t;
/* -------- cgreenlet -------- */
#ifdef USE_CGREENLET
#include "greenlet.h"
typedef greenlet_t coroutine_t;
#endif

#ifdef USE_COROUTINE
#include "coroutine.h"
#endif

typedef enum SockmsgState{
    SOCKMSG_WAITING_HEAD = 0,
    SOCKMSG_WAITING_BODY
} SockmsgState;

typedef struct sockmsg_t{
    enum SockmsgState state;
    uint32_t except_bytes;

    message_t message_header;
    message_t *message;

} sockmsg_t;

sockmsg_t *sockmsg_new(void);
void sockmsg_free(sockmsg_t *sockmsg);

/* -------------------- sockbuf_t -------------------- */
typedef struct sockbuf_t {
    uint32_t len;
    uint32_t read_head;
    uint32_t write_head;
    uint32_t remain_bytes;
    uint32_t blockid;
    session_t *session;
    char base[DEFAULT_SOCKBUF_SIZE];
} sockbuf_t;

void sockbuf_init(sockbuf_t *sockbuf);
sockbuf_t *sockbuf_new(session_t *session);
void sockbuf_free(sockbuf_t *sockbuf);

/* -------------------- connection_t -------------------- */
typedef struct connection_t {
  //session_t *session;  [> Backlink to owning client context. <]
  uv_loop_t loop;
  union {
    uv_handle_t handle;
    uv_stream_t stream;
    uv_tcp_t tcp;
    uv_udp_t udp;
  } handle;

  uint64_t total_bytes; 

  //unsigned char rdstate;
  //unsigned char wrstate;
  //unsigned int idle_timeout;

  //ssize_t result;
  //uv_timer_t timer_handle;  [> For detecting timeouts. <]
  //uv_write_t write_req;
  //[> We only need one of these at a time so make them share memory. <]
  //union {
    //uv_getaddrinfo_t addrinfo_req;
    //uv_connect_t connect_req;
    //uv_req_t req;
    //struct sockaddr_in6 addr6;
    //struct sockaddr_in addr4;
    //struct sockaddr addr;
  //} t;
} connection_t;

int connection_init(connection_t *connection);
void connection_destroy(connection_t *connection);

/* -------------------- session_id -------------------- */
//typedef struct session_id {
	//union {
		//struct {
			//uint32_t nodeid;
			//uint32_t seq_no;
		//};
		//uint64_t id;
	//};
//} session_id;

typedef enum ConsumeState{
    ConsumeState_WAITING_HEAD = 0,
    ConsumeState_WAITING_BODY
} ConsumeState;

typedef struct message_context_t {
    message_t msgheader;
    message_t *message;
    int current_state;
    uint32_t consumed_size;

    uint32_t except_size;

    uint32_t writed_body_size;

    uint32_t total_message_new;
    uint32_t total_message_free;
} message_context_t;


typedef void (*session_after_write_request_cb)(session_t*, int);

/* -------------------- session_t -------------------- */
typedef struct session_t{
    //session_id sid;
    connection_t connection;  /* Connection with the SOCKS client. */
    service_t *service;

    sockbuf_t last_sockbuf;
    //void *react_ctx;
    //int react_id_session;

    message_context_t msgctx;

    sockmsg_t *sockmsg;

    uint32_t id;
    void *user_data;

    session_after_write_request_cb after_write_request;

    //session_callbacks_t callbacks;

    int stop; /* if true, the connection is not ready for read
               * operations because of too many requests */
    int waiting_for_close;
    uint32_t total_writed;
    uint32_t total_readed;
    uint32_t cached_bytes;
    uint32_t total_blocks; /* for build blockid */


    //sockbuf_t *sockbuf;


    uv_idle_t idle_handle;
    uv_timer_t timer_handle;
    uv_async_t async_handle;

    /* private fields */
#ifdef USE_CGREENLET
    coroutine_t *rx_coroutine;
    coroutine_t *tx_coroutine; 
#endif

#ifdef USE_LIBCORO
    coro_context main_coctx;
    //struct coro_stack main_stack;
    char main_stack[64*1024]; 
    coro_context rx_coctx;
    //struct coro_stack rx_stack;
    char rx_stack[64*1024];
#endif

#ifdef USE_COROUTINE
    coroutine_t *rx_coctx;
#endif

    uint32_t total_received_buffers;
    uint32_t total_saved_buffers;

    list *responseQueue;

    uint32_t finished_works;
    uint32_t total_finished_works;
    uint32_t running_tasks;
    
    pthread_mutex_t recv_pending_lock;
    pthread_cond_t recv_pending_cond;

    pthread_mutex_t send_pending_lock;
    pthread_cond_t send_pending_cond;

} session_t;

#define session_handle(session) \
    session->connection.handle.handle

#define session_stream(session) \
    session->connection.handle.stream

#define session_tcp(session) \
    session->connection.handle.tcp

#define session_udp(session) \
    session->connection.handle.udp

//extern session_t* session_new(service_t *service, const session_callbacks_t *callbacks, void *user_data);
extern session_t* session_new(service_t *service, void *user_data);
extern void session_free(session_t *session);
extern int session_accept(session_t *session, uv_tcp_t *parent_tcp);

int session_listen(service_t *service, int listen_port);
int session_loop(session_t *session, const char *ip, int port);
int session_waiting_message(session_t *session);

extern int session_rx_on(session_t *session);
extern void session_rx_off(session_t *session);

extern void session_shutdown(session_t *session);
extern void session_after_shutdown(uv_shutdown_t *shutdown_req, int status);

extern int too_many_requests(session_t *session);

extern int session_send_data(session_t *session, char *buf, uint32_t buf_size, void *user_data, uv_write_cb after_write);
extern int session_response_data(session_t *session, char *buf, uint32_t buf_size);
extern void session_response_message(session_t *session, message_t *message);
extern void session_response(session_t *session, enum MSG_RESULT result); 

sockbuf_t *get_current_sockbuf(void);
void set_current_sockbuf(sockbuf_t *sockbuf);

//int session_write_request(session_t *session, void *data, uint32_t data_size, uv_write_cb after_write);
int session_write_request(session_t *session, void *data, uint32_t data_size, session_after_write_request_cb after_write_request);

#define WARNING_LOG_SESSION_SOCKBUF(msg) \
    warning_log("\n........\n %s fd(%d) block(%d) read_head=%d write_head=%d, remain_bytes=%d\n", msg, session_fd(session), sockbuf->blockid, sockbuf->read_head, sockbuf->write_head, sockbuf->remain_bytes);


#define TRACE_LOG_SESSION_SOCKBUF(msg) \
    trace_log("\n........\n %s fd(%d) block(%d) read_head=%d write_head=%d, remain_bytes=%d\n", msg, session_fd(session), sockbuf->blockid, sockbuf->read_head, sockbuf->write_head, sockbuf->remain_bytes);

#define TRACE_session(msg) \
        trace_log(msg "fd(%d) waiting_for_close=%d total_received_buffers=%d total_saved_buffers=%d session_cached_bytes=%d ", \
                session_fd(session), \
                session->waiting_for_close, \
                session->total_received_buffers, \
                session->total_saved_buffers, \
                session->cached_bytes \
                ); \

#define SESSION_LOOP(session) \
    &(session->connection.loop)

#define SESSION_TCP(session) \
    &(session->connection.handle.tcp)

/* Get file descripter in uv_stream_t. */
#define stream_fd(stream) \
    stream->io_watcher.fd

/* Get file descripter in server_t. */
#define server_fd(server) \
    server->service->connection.handle.tcp.io_watcher.fd

/* Get file descripter in session_t. */
#define session_fd(session) \
    session->service->connection.handle.stream.io_watcher.fd

/* Get file descripter in session_t. */
#define client_fd(client) \
    client->connect_req.handle->io_watcher.fd

/* Get file descripter in session_t. */
#define client_fd1(client) \
    client->connection.handle.stream.io_watcher.fd

#endif /* __SESSION_H__ */

