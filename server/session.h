#ifndef __SESSION_H__
#define __SESSION_H__

#include "legolas.h"

#define MAX_CACHED_BYTES 1024 * 1024 * 1024

extern session_t* create_session(server_t* server);
extern void close_session(session_t *session);

extern void session_incref(struct session_t *session);
extern void session_decref(struct session_t *session);

extern int session_rx_on(session_t *session);
extern void session_rx_off(session_t *session);
extern int session_tx_on(session_t *session);
extern void session_tx_off(session_t *session);

extern int too_many_requests(session_t *session);
extern int session_is_waiting(session_t *session);
extern void session_finish_saving_buffer(session_t *session);

extern void init_cob(conn_buf_t *cob);
extern void delete_cob(conn_buf_t *cob);

extern void enqueue_recv_queue(session_t *session, conn_buf_t *cob); 
extern conn_buf_t *dequeue_recv_queue(session_t *session); 

#define session_FROM_UV_HANDLE(handle, session_iinfo, server) \
    UNUSED session_t *session = (struct session_t *)handle->data; \
    UNUSED server_t *server = session->server;

#define WARNING_LOG_SESSION_COB(msg) \
    warning_log("\n........\n %s fd(%d) block(%d) read_head=%d write_head=%d, remain_bytes=%d\n", msg, session_fd(session), cob->blockid, cob->read_head, cob->write_head, cob->remain_bytes);


#define TRACE_LOG_SESSION_COB(msg) \
    trace_log("\n........\n %s fd(%d) block(%d) read_head=%d write_head=%d, remain_bytes=%d\n", msg, session_fd(session), cob->blockid, cob->read_head, cob->write_head, cob->remain_bytes);

#define TRACE_session(msg) \
        trace_log(msg "fd(%d) refcnt=%d waiting_for_close=%d total_received_buffers=%d total_saved_buffers=%d session_cached_bytes=%d server_cached_bytes=%d", \
                session_fd(session), \
                session->refcnt, \
                session->waiting_for_close, \
                session->total_received_buffers, \
                session->total_saved_buffers, \
                session->cached_bytes, \
                server->cached_bytes \
                ); \

#endif /* __SESSION_H__ */

