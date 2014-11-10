/**
 * @file   everdata.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-10 14:58:16
 * 
 * @brief  
 * 
 * 
 */

#include <czmq.h>
#include "everdata.h"

void message_add_status(zmsg_t *msg, const char *status)
{
    int16_t msgtype = MSGTYPE_STATUS;
    zmsg_addmem(msg, &msgtype, sizeof(int16_t));
    zmsg_addmem(msg, status, strlen(status));
}

void message_add_heartbeat(zmsg_t *msg, const char *heartbeat)
{
    int16_t msgtype = MSGTYPE_HEARTBEAT;
    zmsg_addmem(msg, &msgtype, sizeof(int16_t));
    zmsg_addmem(msg, heartbeat, strlen(heartbeat));
}

zmsg_t *create_base_message(int16_t msgtype)
{
    zmsg_t *msg = zmsg_new();
    zmsg_addmem(msg, &msgtype, sizeof(int16_t));

    return msg;
}

zmsg_t *create_status_message(const char *status)
{
    zmsg_t *msg = create_base_message(MSGTYPE_STATUS);
    zmsg_addstr(msg, status);

    return msg;
}

zmsg_t *create_heartbeat_message(const char *heartbeat)
{
    zmsg_t *msg = create_base_message(MSGTYPE_HEARTBEAT);
    zmsg_addstr(msg, heartbeat);

    return msg;
}

zmsg_t *create_key_data_message(const char *key, const char *data, uint32_t data_size)
{
    zmsg_t *msg = create_base_message(MSGTYPE_DATA);
    zmsg_addstr(msg, key);
    zmsg_addmem(msg, data, data_size);

    return msg;
}

zmsg_t *create_sendback_message(zmsg_t *msg)
{
    zframe_t *cli_identity = zmsg_unwrap(msg);

    zmsg_t *sendback_msg = zmsg_new();
    zmsg_wrap(sendback_msg, cli_identity);

    return sendback_msg;
}

zmsg_t *create_data_message(const char *data, uint32_t data_size)
{
    zmsg_t *msg = create_base_message(MSGTYPE_DATA);
    zmsg_addmem(msg, data, data_size);

    return msg;
}

int __message_send_data(zsock_t *sock, int16_t msgtype, const char *data, uint32_t data_size)
{
    /*zmsg_t *msg = zmsg_new();*/

    /*zmsg_addmem(msg, &msgtype, sizeof(int16_t));*/
    /*zmsg_addmem(msg, data, data_size);*/
    zmsg_t *msg = create_base_message(msgtype);
    zmsg_addmem(msg, data, data_size);
    
    int rc = zmsg_send(&msg, sock);

    return rc;
}

int __message_send_str(zsock_t *sock, int16_t msgtype, const char *data)
{
    /*zmsg_t *msg = zmsg_new();*/

    /*zmsg_addmem(msg, &msgtype, sizeof(int16_t));*/
    /*zmsg_addstr(msg, data);*/
    
    zmsg_t *msg = create_base_message(msgtype);
    zmsg_addstr(msg, data);

    int rc = zmsg_send(&msg, sock);

    return rc;
}

int message_send_status(zsock_t *sock, const char *status)
{
    return __message_send_str(sock, MSGTYPE_STATUS, status);
}

int message_send_heartbeat(zsock_t *sock, const char *heartbeat)
{
    return __message_send_str(sock, MSGTYPE_HEARTBEAT, heartbeat);
}

int message_send_data(zsock_t *sock, const char *data, uint32_t data_size)
{
    return __message_send_data(sock, MSGTYPE_DATA, data, data_size);
}

int message_send_str(zsock_t *sock, const char *data)
{
    return __message_send_str(sock, MSGTYPE_DATA, data);
}

int16_t message_get_msgtype(zmsg_t *msg){
    zframe_t *frame_msgtype = zmsg_first(msg);
    if ( frame_msgtype != NULL && zframe_size(frame_msgtype) == sizeof(int16_t) ){
        return *(int16_t*)zframe_data(frame_msgtype);
    } 

    return MSGTYPE_UNKNOWN;
}

int message_check_status(zmsg_t *msg, const char *status)
{
    int16_t msgtype = message_get_msgtype(msg);
    if ( msgtype == MSGTYPE_STATUS ){
        zmsg_first(msg);
        zframe_t *frame = zmsg_next(msg);
        if ( frame != NULL ){
            return memcmp(zframe_data(frame), status, strlen(status));
        }
    }
    return -1;
}

int message_check_heartbeat(zmsg_t *msg, const char *heartbeat)
{
    int16_t msgtype = message_get_msgtype(msg);
    if ( msgtype == MSGTYPE_HEARTBEAT ){
        zmsg_first(msg);
        zframe_t *frame = zmsg_next(msg);
        if ( frame != NULL ){
            return memcmp(zframe_data(frame), heartbeat, strlen(heartbeat));
        }
    }
    return -1;
}

