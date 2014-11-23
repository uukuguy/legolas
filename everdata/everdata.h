/**
 * @file   everdata.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-10 14:41:58
 * 
 * @brief  
 * 
 * 
 */
#ifndef __EVERDATA_H__
#define __EVERDATA_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define BROKER_FRONTEND_PORT 19977
#define BROKER_BACKEND_PORT 19978
    
#define HEARTBEAT_INTERVAL 1000
#define HEARTBEAT_LIVENESS 5
#define INTERVAL_INIT 1000
#define INTERVAL_MAX 32000

#define MSGTYPE_UNKNOWN 0x00FF
#define MSGTYPE_STATUS 0x00FE
#define MSGTYPE_DATA    0x00FD
#define MSGTYPE_HEARTBEAT 0x00FC
#define MSGTYPE_ACTION 0x00FB

#define MSG_HEARTBEAT_WORKER    "\x00\x0A"
#define MSG_HEARTBEAT_BROKER    "\x00\x0B"
#define MSG_HEARTBEAT_CLIENT    "\x00\x0C"

#define MSG_STATUS_ACTOR_READY  "\x01\x01"
#define MSG_STATUS_ACTOR_OVER   "\x01\xFF"

#define MSG_STATUS_WORKER_READY    "\x0A\x00"
#define MSG_STATUS_WORKER_ACK      "\x0A\x01"
#define MSG_STATUS_WORKER_NOTFOUND "\x0A\x02"
#define MSG_STATUS_WORKER_PENDING  "\x0A\xFE"
#define MSG_STATUS_WORKER_ERROR    "\x0A\xFF"

#define MSG_STATUS_BROKER_READY "\x0B\x00"
#define MSG_STATUS_BROKER_ACK   "\x0B\x01"
#define MSG_STATUS_BROKER_PENDING "\x0B\xFE"
#define MSG_STATUS_BROKER_ERROR "\x0B\xFF"

#define MSG_STATUS_CLIENT_READY "\x0C\x00"
#define MSG_STATUS_CLIENT_ACK   "\x0C\x01"
#define MSG_STATUS_CLIENT_PENDING "\x0C\xFE"
#define MSG_STATUS_CLIENT_ERROR "\x0C\xFF"

#define MSG_ACTION_PUT "\x02\x01"
#define MSG_ACTION_GET "\x02\x02"
#define MSG_ACTION_DEL "\x02\x03"

typedef struct _zsock_t zsock_t;
typedef struct _zmsg_t zmsg_t;

int16_t message_get_msgtype(zmsg_t *msg);
int message_check_status(zmsg_t *msg, const char *status);
int message_check_heartbeat(zmsg_t *msg, const char *heartbeat);
int message_check_action(zmsg_t *msg, const char *action);
void message_add_status(zmsg_t *msg, const char *status);
void message_add_heartbeat(zmsg_t *msg, const char *heartbeat);
void message_add_key_data(zmsg_t *msg, const char *key, const char *data, uint32_t data_size);
int message_send_status(zsock_t *sock, const char *status);
int message_send_heartbeat(zsock_t *sock, const char *heartbeat);

zmsg_t *create_base_message(int16_t msgtype);
zmsg_t *create_status_message(const char *status);
zmsg_t *create_heartbeat_message(const char *heartbeat);
zmsg_t *create_action_message(const char *action);
zmsg_t *create_data_message(const char *data, uint32_t data_size);
zmsg_t *create_key_data_message(const char *key, const char *data, uint32_t data_size);
zmsg_t *create_sendback_message(zmsg_t *msg);


#ifdef __cplusplus
}
#endif

#endif /* __EVERDATA_H__ */

