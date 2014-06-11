/**
 * @file   operation.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-05-05 19:12:38
 * 
 * @brief  
 * 
 * 
 */

#include "operation.h"
#include "logger.h"

static int exec_read_request(const struct msg_request_t *request, struct msg_response_t **rsp, struct session_info_t *session_info){
    int ret = 0;

    return ret;
}

static int exec_write_request(const struct msg_request_t *request, struct msg_response_t **rsp, struct session_info_t *session_info){
    int ret = 0;

    return ret;
}

static int exec_del_request(const struct msg_request_t *request, struct msg_response_t **rsp, struct session_info_t *session_info){
    int ret = 0;

    return ret;
}

static void notify_write_event(const struct msg_request_t *request){
}

static void notify_del_event(const struct msg_request_t *request){
}

static struct operation_handler_t operations[] = {
	{
		.op_code = MSG_OP_READ,
		.exec_request = exec_read_request,
		.notify_event = NULL,
	}, {
		.op_code = MSG_OP_WRITE,
		.exec_request = exec_write_request,
		.notify_event = notify_write_event,
	}, {
		.op_code = MSG_OP_DEL,
		.exec_request = exec_del_request,
		.notify_event = notify_del_event,
    }
}; /* struct operation_handler_t operations[] */

struct operation_handler_t *find_operation(enum MSG_OPERATION op_code){
	int i;
	for (i = 0; i < ARRAY_SIZE(operations); i++) {
		if (op_code == operations[i].op_code)
			return operations + i;
	}
	error_log("no such operation, %d\n", op_code);
    return NULL;
}

