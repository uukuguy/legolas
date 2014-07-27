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

#include "message.h"
#include "session.h"

typedef struct operation_handler_t {

    enum MSG_OPERATION op_code;

    int (*exec_request)(const message_t *request, message_t **response, session_t *session);
    void (*notify_event)(const message_t *request);

} operation_handler_t;

struct operation_handler_t *find_operation(enum MSG_OPERATION op_code);

#endif /* __OPERATION_H__ */

