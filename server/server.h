#ifndef __SERVER_H__
#define __SERVER_H__

#include "common.h"
#include "storage.h"
#include <uv.h>

#define RECV_THREADS 4
#define SEND_THREADS 4
#define RECV_INTERVAL 1 /* ms */
#define SEND_INTERVAL 1 /* ms */

#define MAX_VNODES 64

struct vnode_info_t;

typedef struct server_info_t {
    unsigned int idle_timeout;  /* Connection idle timeout in ms. */
    uv_tcp_t tcp_handle;

    struct work_queue *recv_queue[RECV_THREADS];
    struct work_queue *send_queue[SEND_THREADS];

    char root_dir[NAME_MAX];

    struct vnode_info_t *vnodes[MAX_VNODES];

    storage_info_t storage_info;

    uint32_t cached_bytes;

} server_info_t;

#endif /* __SERVER_H__ */

