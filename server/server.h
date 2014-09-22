#ifndef __SERVER_H__
#define __SERVER_H__

#include "common.h"
#include "storage.h"
#include "work.h"
#include "session.h"
#include <uv.h>

#define RECV_THREADS 4 
#define SEND_THREADS 4 
#define RECV_INTERVAL 1 /* ms */
#define SEND_INTERVAL 1 /* ms */

#define LOGFILES 4
#define LOG_INTERVAL 1000 /* 1000ms */

#define VNODES 64

typedef struct session_t session_t;
typedef struct vnode_t vnode_t;
typedef struct object_t object_t;
typedef struct logfile_t logfile_t;
typedef struct md5_value_t md5_value_t;

typedef struct server_t {
    connection_t connection;

    unsigned int idle_timeout;  /* Connection idle timeout in ms. */


    work_queue_t *recv_queue[RECV_THREADS];
    work_queue_t *send_queue[SEND_THREADS];

    logfile_t *logfiles[LOGFILES];

    char root_dir[NAME_MAX];

    vnode_t *vnodes[VNODES];

    storage_t storage;

    uint32_t cached_bytes;

    pthread_mutex_t send_pending_lock;
    pthread_cond_t send_pending_cond;

    uint32_t total_requests;

} server_t;

#define SERVER(session) (server_t*)(session->parent)
#define server(session) (server_t*)(session->parent)

server_t *server_new(void);
void server_free(server_t *server);

vnode_t *get_vnode_by_key(server_t *server, md5_value_t *key_md5);
logfile_t *get_logfile_by_session(server_t *server, session_t *session);
work_queue_t *get_recv_queue_by_session(server_t *server, session_t *session);
work_queue_t *get_send_queue_by_session(server_t *server, session_t *session);

#endif /* __SERVER_H__ */

