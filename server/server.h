#ifndef __SERVER_H__
#define __SERVER_H__

#include "common.h"
#include "storage.h"
#include "work.h"
#include "service.h"
#include "session.h"
#include "vnode.h"
#include <uv.h>

#define RECV_INTERVAL 1 /* ms */
#define SEND_INTERVAL 1 /* ms */

#define LOGFILES 4
#define LOG_INTERVAL 1000 /* 1000ms */

//#define VNODES 64
#define VNODES 4

typedef struct service_t service_t;
typedef struct session_t session_t;
typedef struct vnode_t vnode_t;
typedef struct object_t object_t;
typedef struct logfile_t logfile_t;
typedef struct md5_value_t md5_value_t;

typedef struct server_options_t {
    int listen_port;
    const char *data_dir;
    enum eVnodeStorageType storage_type;
} server_options_t;

typedef struct server_t {
    service_t *service;

    connection_t connection;

    unsigned int idle_timeout;  /* Connection idle timeout in ms. */

    server_options_t options;

    work_queue_t **recv_queue;
    work_queue_t **send_queue;

    logfile_t *logfiles[LOGFILES];

    char root_dir[NAME_MAX];

    vnode_t *vnodes[VNODES];
    enum eVnodeStorageType storage_type;
    uint32_t num_processors;
    uint32_t recv_threads;
    uint32_t send_threads;

    char data_dir[NAME_MAX];

    uint32_t cached_bytes;

    pthread_mutex_t send_pending_lock;
    pthread_cond_t send_pending_cond;

    uint32_t total_requests;

} server_t;

#define SERVER(session) (server_t*)(session->service->parent)
#define server(session) (server_t*)(session->service->parent)

server_t *server_new(const server_options_t *server_options);
void server_free(server_t *server);

int server_init(server_t *server);
int server_listen(server_t *server);

vnode_t *get_vnode_by_key(server_t *server, md5_value_t *key_md5);
logfile_t *get_logfile_by_session(server_t *server, session_t *session);

int server_write_to_storage(session_t *session, object_t *object);

#endif /* __SERVER_H__ */

