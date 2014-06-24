/**
 * @file   operation.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-05-05 19:06:51
 * 
 * @brief  
 * 
 * 
 */

#ifndef __OPERATION_H__
#define __OPERATION_H__

#include "protocol.h"
#include "legolas.h"

typedef struct operation_handler_t {

    enum MSG_OPERATION op_code;

    int (*exec_request)(const struct msg_request_t *request, struct msg_response_t **response, struct session_t *session);
    void (*notify_event)(const struct msg_request_t *request);

} operation_handler_t;

struct operation_handler_t *find_operation(enum MSG_OPERATION op_code);

#endif /* __OPERATION_H__ */

