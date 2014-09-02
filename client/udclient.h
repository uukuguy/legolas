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
#include <pthread.h>

typedef struct session_t session_t;

typedef struct udclient_t {
    uint32_t id;
    pthread_t tid;
    uint32_t errno;
    session_t *session;
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

