#ifndef __SESSION_H__
#define __SESSION_H__

#include "legolas.h"

extern session_info_t* create_session(server_info_t* server_info);
extern void close_session(session_info_t *session_info);

extern void session_incref(struct session_info_t *session_info);
extern void session_decref(struct session_info_t *session_info);

extern int session_rx_on(session_info_t *session_info);
extern void session_rx_off(session_info_t *session_info);
extern int session_tx_on(session_info_t *session_info);
extern void session_tx_off(session_info_t *session_info);

extern int too_many_requests(session_info_t *session_info);
extern int session_is_waiting(session_info_t *session_info);
extern void session_finish_saving_buffer(session_info_t *session_info);

extern void init_cob(conn_buf_t *cob);
extern void destroy_cob(conn_buf_t *cob);

extern void queue_into_recv_queue(session_info_t *session_info, conn_buf_t *cob); 
extern void remove_from_recv_queue(session_info_t *session_info, conn_buf_t *cob); 

#define SESSION_INFO_FROM_UV_HANDLE(handle, session_iinfo, server_info) \
    UNUSED session_info_t *session_info = (struct session_info_t *)handle->data; \
    UNUSED server_info_t *server_info = session_info->server_info;

#define WARNING_LOG_SESSION_COB(msg) \
    warning_log("\n........\n %s fd(%d) block(%d) read_head=%d write_head=%d, remain_bytes=%d\n", msg, session_fd(session_info), cob->blockid, cob->read_head, cob->write_head, cob->remain_bytes);


#define TRACE_LOG_SESSION_COB(msg) \
    trace_log("\n........\n %s fd(%d) block(%d) read_head=%d write_head=%d, remain_bytes=%d\n", msg, session_fd(session_info), cob->blockid, cob->read_head, cob->write_head, cob->remain_bytes);

#define TRACE_SESSION_INFO(msg) \
        trace_log(msg "fd(%d) refcnt=%d waiting_for_close=%d total_received_buffers=%d total_saved_buffers=%d session_cached_bytes=%d server_cached_bytes=%d", \
                session_fd(session_info), \
                session_info->refcnt, \
                session_info->waiting_for_close, \
                session_info->total_received_buffers, \
                session_info->total_saved_buffers, \
                session_info->cached_bytes, \
                server_info->cached_bytes \
                ); \

#endif /* __SESSION_H__ */

