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

typedef struct list list;
typedef struct udclient_t udclient_t;
typedef struct session_t session_t;
typedef struct message_t message_t;
typedef struct msgidx_t msgidx_t;

typedef int (*on_ready_cb)(udclient_t *udcli);
typedef int (*after_write_finished_cb)(udclient_t *udcli, message_t *response);
typedef int (*after_read_finished_cb)(udclient_t *udcli, message_t *response);
typedef int (*after_delete_finished_cb)(udclient_t *udcli, message_t *response);
typedef int (*after_write_object_slice_cb)(udclient_t *udcli, msgidx_t *msgidx);
typedef int (*after_read_object_slice_cb)(udclient_t *udcli, msgidx_t *msgidx);

typedef struct udclient_t {
    uint32_t id;
    pthread_t tid;
    uint32_t err;
    session_t *session;
    void *user_data;

    int op_code;
    char key[NAME_MAX];
    uint32_t keylen;
    uint32_t object_size;
    uint32_t total_readed;
    uint32_t total_writed;

    uint32_t slice_idx;
    uint32_t nslices;
    char *data;
    uint32_t data_size;

    list *writing_objects;

    on_ready_cb on_ready;
    after_write_finished_cb after_write_finished;
    after_read_finished_cb after_read_finished;
    after_delete_finished_cb after_delete_finished;

    after_write_object_slice_cb after_write_object_slice;
    after_read_object_slice_cb after_read_object_slice;

    pthread_mutex_t on_ready_lock;
    pthread_cond_t on_ready_cond;

} udclient_t;

udclient_t *udclient_new(void *user_data);
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

