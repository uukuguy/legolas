/**
 * @file   udclient.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-09-09 13:29:23
 * 
 * @brief  
 * 
 * 
 */

#ifndef __UDCLIENT_H__
#define __UDCLIENT_H__

#include <stdint.h>
#include <limits.h>
#include <pthread.h>

typedef struct udclient_t udclient_t;
typedef struct session_t session_t;
typedef struct message_t message_t;
typedef struct msgidx_t msgidx_t;

typedef int (*after_write_finished_cb)(udclient_t *udcli, message_t *response);
typedef int (*after_read_finished_cb)(udclient_t *udcli, message_t *response);
typedef int (*after_delete_finished_cb)(udclient_t *udcli, message_t *response);
typedef int (*after_read_object_slice_cb)(udclient_t *udcli, msgidx_t *msgidx, message_t *response);

typedef struct udclient_t {
    uint32_t id;
    pthread_t tid;
    uint32_t err;
    session_t *session;

    int op_code;
    char key[NAME_MAX];
    uint32_t keylen;
    uint32_t object_size;
    uint32_t total_readed;
    uint32_t total_writed;

    after_write_finished_cb after_write_finished;
    after_read_finished_cb after_read_finished;
    after_delete_finished_cb after_delete_finished;
    after_read_object_slice_cb after_read_object_slice;

} udclient_t;

udclient_t *udclient_new(void);
void udclient_free(udclient_t *udcli);
int udclient_run(udclient_t *udcli);
void udclient_exit(udclient_t *udcli);

int udclient_open_data(udclient_t *udcli, const char *key);
int udclient_write_data(udclient_t *udcli, int handle, void *data, uint32_t len);
int udclient_read_data(udclient_t *udcli, int handle, void *data, uint32_t len);
int udclient_close_data(udclient_t *udcli, int handle);
int udclient_delete_data(udclient_t *udcli, const char *key);



#define UDCLIENT(session) (udclient_t*)(session->parent)
#define UDCLIENT_USER_DATA(session) (void*)(session->user_data)

#endif /* __UDCLIENT_H__ */

