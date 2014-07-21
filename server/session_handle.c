/**
 * @file   session_handle.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-07-07 16:15:19
 * 
 * @brief  
 * 
 * 
 */

#include "session.h"
#include "logger.h"

int session_handle_write(session_t *session, msg_request_t *request);
int session_handle_read(session_t *session, msg_request_t *request);
int session_handle_delete(session_t *session, msg_request_t *request);

int session_handle_request(session_t *session, msg_request_t *request)
{
    int ret = 0;

    switch ( request->op_code ){
        case MSG_OP_WRITE:
            {
                trace_log("MSG_OP_WRITE");
                ret = session_handle_write(session, request);
            } break;
        case MSG_OP_READ:
            {
                trace_log("MSG_OP_READ");
                ret = session_handle_read(session, request);
            } break;
        case MSG_OP_DEL:
            {
                trace_log("MSG_OP_DEL");
                ret = session_handle_delete(session, request);
            } break;
    };

    return ret;
}

