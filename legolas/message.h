/**
 * @file   message.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-07-07 00:59:00
 * 
 * @brief  
 * 
 * 
 */

#ifndef __MESSAGE_H__
#define __MESSAGE_H__

#include "common.h"
#include "util.h"
#include "zmalloc.h"
#include "md5.h"

#define PROTOCAL_VERSION 1

static const char magic_code[] = "legolas";

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
	RESULT_ERR_UNKNOWN = 0x10,

    RESULT_ERR_NOTFOUND = 0x20,
    RESULT_ERR_STORAGE_FAILED = 0x40
};

/* -------------------- MESSAGE_HEADER_FIELDS -------------------- */ 
#define MESSAGE_HEADER_FIELDS \
    uint8_t         magic_code[8]; \
	uint32_t        id; \
	uint8_t         msg_type; \
	uint8_t         msg_version; \
    uint8_t         op_code; \
    uint8_t         result; \
    uint16_t        reserved; \
    /* entire data length without header */ \
	uint32_t        data_length 


/* -------------------- message_t -------------------- */ 
typedef struct message_t {
    MESSAGE_HEADER_FIELDS;

    uint8_t data[0];
} message_t;

/* -------------------- message_arg_t -------------------- */ 
typedef struct message_arg_t {
	uint32_t        size;
	char            data[0];
} message_arg_t;

#define message_next_arg(arg0) \
            (struct message_arg_t *)((uint8_t *)arg0 + sizeof(arg0->size) + arg0->size) 

extern message_t *alloc_request_message(uint32_t id, enum MSG_OPERATION op_code);
extern message_t *alloc_response_message(uint32_t id, enum MSG_RESULT result);
extern message_t *add_message_arg(message_t *message, const void *data, uint32_t data_len);
extern int check_message(message_t *message);
extern int check_data_crc32(int requestid, message_arg_t *argCRC32, message_arg_t *argData);


typedef struct msgidx_t {
    message_t *message;
    md5_value_t *key_md5;
    uint32_t object_size;
    uint32_t slice_idx;
    uint32_t nslices;

    const char *key;
    uint32_t keylen;
    const char *data;
    uint32_t data_size;

} msgidx_t;

msgidx_t *msgidx_new(void);
void msgidx_init(msgidx_t *msgidx);
void msgidx_free(msgidx_t *msgidx);

#endif /* __MESSAGE_H__ */

