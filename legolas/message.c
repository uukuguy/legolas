/**
 * @file   message.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-07-07 01:07:27
 * 
 * @brief  
 * 
 * 
 */
#include "message.h"
#include "logger.h"
#include "crc32.h"

message_t *alloc_request_message(uint32_t id, enum MSG_OPERATION op_code)
{
    message_t *message = (message_t*)zmalloc(sizeof(message_t));
    memset(message, 0, sizeof(message_t));
    memcpy(message->magic_code, magic_code, sizeof(magic_code));
    message->msg_version = PROTOCAL_VERSION;
    message->msg_type = MSG_TYPE_REQUEST;
    message->id = id;
    message->op_code = op_code;
    return message;
}

message_t *alloc_response_message(uint32_t id, enum MSG_RESULT result)
{
    message_t *message = (message_t*)zmalloc(sizeof(message_t));
    memset(message, 0, sizeof(message_t));
    memcpy(message->magic_code, magic_code, sizeof(magic_code));
    message->msg_version = PROTOCAL_VERSION;
    message->msg_type = MSG_TYPE_RESPONSE;
    message->id = id;
    message->result = result;
    return message;
}

message_t *add_message_arg(message_t *message, const void *data, uint32_t data_len)
{
    message = (message_t*)zrealloc(message, sizeof(*message) + message->data_length +
            sizeof(data_len) + data_len);
    if (unlikely(!message)) {
        error_log("add_message_arg() failed.");
        return NULL;
    }

    struct message_arg_t *arg;
    arg = (struct message_arg_t *)(message->data + message->data_length);
    arg->size = data_len;
    memcpy(arg->data, data, data_len);

    message->data_length += sizeof(data_len) + data_len;

    return message;
}

int check_message(message_t *message)
{
    if ( message->magic_code[0] == 'l' &&
         message->magic_code[1] == 'e' &&
         message->magic_code[2] == 'g' &&
         message->magic_code[3] == 'o' &&
         message->magic_code[4] == 'l' &&
         message->magic_code[5] == 'a' &&
         message->magic_code[6] == 's' ) { 
        return 0;
    } else
        return -1;
}

/* ==================== check_data_crc32() ==================== */ 
int check_data_crc32(int requestid, message_arg_t *argCRC32, message_arg_t *argData)
{
    /*return 0;*/

    uint32_t crc = *((uint32_t*)argCRC32->data);
    uint32_t crc1 = crc32(0, argData->data, argData->size);

    if ( crc != crc1 ) {
        error_log("requestid(%d) upload crc32: %d, Data crc32: %d", requestid, crc, crc1);
        return -1;
    }

    return 0;
}

