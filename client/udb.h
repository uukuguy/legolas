/**
 * @file   udb.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-09-09 13:29:23
 * 
 * @brief  
 * 
 * 
 */

#ifndef __udb_H__
#define __udb_H__

#include <stdint.h>
#include <limits.h>
#include <pthread.h>
#include "service.h"

typedef struct list list;
typedef struct udb_t udb_t;
typedef struct service_t service_t;
typedef struct session_t session_t;
typedef struct message_t message_t;
typedef struct msgidx_t msgidx_t;

typedef int (*on_ready_cb)(udb_t *udb);
typedef int (*after_write_finished_cb)(udb_t *udb, message_t *response);
typedef int (*after_read_finished_cb)(udb_t *udb, message_t *response);
typedef int (*after_delete_finished_cb)(udb_t *udb, message_t *response);
typedef int (*after_write_object_slice_cb)(udb_t *udb, msgidx_t *msgidx);
typedef int (*after_read_object_slice_cb)(udb_t *udb, msgidx_t *msgidx);

typedef struct udb_t {
    const char *ip;
    int port;

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

    service_t *service;

    //list *writing_objects;

    on_ready_cb on_ready;
    after_write_finished_cb after_write_finished;
    after_read_finished_cb after_read_finished;
    after_delete_finished_cb after_delete_finished;

    after_write_object_slice_cb after_write_object_slice;
    after_read_object_slice_cb after_read_object_slice;

    pthread_mutex_t on_ready_lock;
    pthread_cond_t on_ready_cond;

    pthread_mutex_t main_pending_lock;
    pthread_cond_t main_pending_cond;

} udb_t;

#define udb_get_id(udb) udb->id
#define udb_get_user_data(udb) udb->user_data
#define udb_get_op_code(udb) udb->op_code

int udb_is_write_done(udb_t *udb);
uint32_t udb_get_writed_bytes(udb_t *udb);
uint32_t udb_get_readed_bytes(udb_t *udb);

int udb_do(udb_t *udb, on_ready_cb on_ready);
void udb_done(udb_t *udb);

udb_t *udb_new(const char *ip, int port, void *user_data);
void udb_free(udb_t *udb);
int udb_run(udb_t *udb);
void udb_exit(udb_t *udb);

int udb_open_data(udb_t *udb, const char *key);
int udb_write_data(udb_t *udb, int handle, void *data, uint32_t len, 
        after_write_object_slice_cb after_write_object_slice, 
        after_write_finished_cb after_write_finished);

int udb_append_data(udb_t *udb, int handle, void *data, uint32_t len);

int udb_read_data(udb_t *udb, int handle, 
        after_read_object_slice_cb after_read_object_slice,
        after_read_finished_cb after_read_finished);

int udb_close_data(udb_t *udb, int handle);
int udb_delete_data(udb_t *udb, after_delete_finished_cb after_delete_finished);



#define udb(session) (udb_t*)(session->service->parent)

#endif /* __udb_H__ */

