/**
 * @file   protocol.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-05-05 17:53:18
 * 
 * @brief  
 * 
 * 
 */

#ifndef __PROTOCOL_H__
#define __PROTOCOL_H__

#include "common.h"
#include "util.h"
#include "zmalloc.h"

#define PROTOCAL_VERSION 1

#define MSG_EVENT_NONE          0x00000000

#define MSG_EVENT_CREATED       0x00000001
#define MSG_EVENT_DELETED       0x00000002
#define MSG_EVENT_CHANGED       0x00000004

enum MSG_TYPE {
    MSG_TYPE_UNKNOWN = 0,
	MSG_TYPE_REQUEST,
	MSG_TYPE_RESPONSE,
	MSG_TYPE_NOTIFICATION
};

enum MSG_OPERATION {
    MSG_OP_NONE = 0,
	MSG_OP_WRITE,
	MSG_OP_READ,
	MSG_OP_DEL
};

enum MSG_RESULT {
    RESULT_SUCCESS = 0,

	/* unknown */
	RESULT_ERR_UNKNOWN = 0x10
};

/**
 * msg format :
 *   msg     |msg_xxx_t|
 *   arg1    |size|data|
 *   arg2    |size|data|
 *
 * Sample : 
 *   write(fd, msg, sizeof(msg));
 *   write(fd, arg1->size, sizeof(uint32_t));
 *   write(fd, arg1->data, sizeof(arg1->size));
 *   write(fd, arg2->size, sizeof(uint32_t));
 *   write(fd, arg2->data, sizeof(arg2->size));
 **/

static const char magic_code[] = "legolas";


/* -------------------- msg_header_t -------------------- */ 
#define COMMON_HEADER_FIELDS \
    uint8_t         magic_code[8]; \
	uint32_t        id; \
	uint8_t         msg_type; \
	uint8_t         msg_version; \
    uint16_t        reserved; \
    /* entire data length without header */ \
	uint32_t        data_length 

typedef struct msg_header_t {
    COMMON_HEADER_FIELDS;

	uint8_t         data[0];
} msg_header_t;

/* -------------------- msg_request_t -------------------- */ 
#define REQ_FIELDS \
    uint8_t op_code; \
    uint8_t reserved1; \
    uint16_t reserved2

typedef struct msg_request_t {
    COMMON_HEADER_FIELDS;
    REQ_FIELDS;

    uint8_t data[0];
} msg_request_t;


/* -------------------- msg_response_t -------------------- */ 
#define RSP_FIELDS \
    uint8_t result; \
    uint8_t reserved1; \
    uint16_t reserved2

typedef struct msg_response_t {
    COMMON_HEADER_FIELDS;
    RSP_FIELDS;

    uint8_t data[0];
} msg_response_t;

/* -------------------- msg_arg_t -------------------- */ 
typedef struct msg_arg_t {
	uint32_t        size;
	char            data[0];
} msg_arg_t;

#define for_each_arg(arg, header)						\
	for (arg = (struct msg_arg_t *)header->data;				\
	     (uint8_t *)arg < (header)->data + (header)->data_length;		\
	     arg = (struct msg_arg_t *)((uint8_t *)arg +			\
				      sizeof(arg->size) + arg->size))

#define next_arg(arg0) \
            (struct msg_arg_t *)((uint8_t *)arg0 + sizeof(arg0->size) + arg0->size) 


extern int check_msg(msg_header_t *header);

extern void init_msg_header(struct msg_header_t *header, enum MSG_TYPE type, uint32_t id);
extern void init_msg_request(struct msg_request_t *request, uint32_t id, enum MSG_OPERATION op_code);
extern struct msg_request_t *alloc_request(uint32_t id, enum MSG_OPERATION op_code);
extern struct msg_request_t *alloc_request_write(uint32_t id, const char *key, uint32_t key_len);
extern struct msg_request_t *alloc_request_read(uint32_t id, const char *key, uint32_t key_len);
extern struct msg_request_t *alloc_request_del(uint32_t id, const char *key, uint32_t key_len);

extern void init_msg_response(struct msg_response_t *response, uint32_t id, enum MSG_RESULT result);
extern struct msg_response_t *alloc_response(uint32_t id, enum MSG_RESULT result);

extern const struct msg_arg_t *get_arg(const void *p, int idx);
extern void *add_request_arg(void *p, const void *data, uint32_t data_len);
extern void *append_request_arg(void *p, const void *data, uint32_t data_len);

#endif /* __PROTOCOL_H__ */

