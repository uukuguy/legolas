/**
 * @file   protocol.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-05-15 16:29:23
 * 
 * @brief  
 * 
 * 
 */
#include "protocol.h"

/* ==================== init_msg_header ==================== */   
void init_msg_header(struct msg_header_t *header, enum MSG_TYPE type, uint32_t id)
{
    memcpy(header->magic_code, magic_code, sizeof(magic_code));
    header->msg_version = PROTOCAL_VERSION;
    header->msg_type = type;
    header->id = id;
}

/* ==================== init_msg_request ==================== */   
void init_msg_request(struct msg_request_t *request, uint32_t id, enum MSG_OPERATION op_code)
{
    memset(request, 0, sizeof(msg_request_t));
    init_msg_header((struct msg_header_t*)request, MSG_TYPE_REQUEST, id);
    request->op_code = op_code;
}

/* ==================== alloc_request ==================== */   
struct msg_request_t *alloc_request(uint32_t id, enum MSG_OPERATION op_code)
{
    struct msg_request_t *request = 
        (struct msg_request_t*)zmalloc(sizeof(msg_request_t));
    init_msg_request(request, id, op_code);
    return request;
}

/* ==================== alloc_request_write ==================== */   
struct msg_request_t *alloc_request_write(uint32_t id, const char *key, uint32_t key_len)
{
    struct msg_request_t *request = alloc_request(id, MSG_OP_WRITE);
    request = add_request_arg(request, key, key_len);
    return request;
}


/* ==================== alloc_request_read ==================== */   
struct msg_request_t *alloc_request_read(uint32_t id, const char *key, uint32_t key_len)
{
    struct msg_request_t *request = alloc_request(id, MSG_OP_READ);
    request = add_request_arg(request, key, key_len);
    return request;
}

/* ==================== alloc_request_del ==================== */   
struct msg_request_t *alloc_request_del(uint32_t id, const char *key, uint32_t key_len)
{
    struct msg_request_t *request = alloc_request(id, MSG_OP_DEL);
    request = add_request_arg(request, key, key_len);
    return request;
}

/* ==================== init_msg_response ==================== */   
void init_msg_response(struct msg_response_t *response, uint32_t id, enum MSG_RESULT result)
{
    memset(response, 0, sizeof(msg_response_t));
    init_msg_header((struct msg_header_t*)response, MSG_TYPE_RESPONSE, id);
    response->result = result;
}

/* ==================== alloc_response ==================== */   
struct msg_response_t *alloc_response(uint32_t id, enum MSG_RESULT result)
{
    struct msg_response_t *response = 
        (struct msg_response_t*)zmalloc(sizeof(msg_response_t));
    init_msg_response(response, id, result);
    return response;
}

/* ==================== check_msg ==================== */   
int check_msg(struct msg_header_t *header)
{
    if ( header->magic_code[0] == 'l' &&
         header->magic_code[1] == 'e' &&
         header->magic_code[2] == 'g' &&
         header->magic_code[3] == 'o' &&
         header->magic_code[4] == 'l' &&
         header->magic_code[5] == 'a' &&
         header->magic_code[6] == 's' ) { 
        return 1;
    } else
        return 0;
}

/* ==================== get_arg ==================== */   
/* Get the idx'th argument */
/*const struct msg_arg_t *get_arg(const void *p, int idx)*/
/*{*/
	/*const struct msg_header_t *header = (struct msg_header_t*)p;*/
	/*const struct msg_arg_t *arg;*/
	/*int i = 0;*/

	/*for_each_arg(arg, header) {*/
		/*if (i == idx)*/
			/*return arg;*/
		/*i++;*/
	/*}*/

	/*return NULL;*/
/*}*/

/* ==================== add_message_arg ==================== */   
/* Add a new argument */
/*void *add_message_arg(void *p, const void *data, uint32_t data_len)*/
/*{*/
	/*struct msg_header_t *header = (struct msg_header_t*)p;*/
	/*struct msg_header_t *header = (struct msg_header_t*)p;*/

	/*header = (struct msg_header_t*)zrealloc(header, sizeof(*header) + header->data_length +*/
			  /*sizeof(data_len) + data_len);*/
	/*header = (struct msg_header_t*)zrealloc(header, sizeof(*header) + header->data_length +*/
			  /*sizeof(data_len) + data_len);*/
	/*if (unlikely(!header)) {*/
		/*fprintf(stderr, "oom\n");*/
		/*return NULL;*/
	/*}*/

	/*struct msg_arg_t *arg;*/
	/*arg = (struct msg_arg_t *)(header->data + header->data_length);*/
	/*arg->size = data_len;*/
	/*memcpy(arg->data, data, data_len);*/

	/*header->data_length += sizeof(data_len) + data_len;*/

	/*return header;*/
/*}*/

msg_request_t *add_request_arg(msg_request_t *request, const void *data, uint32_t data_len)
{
	request = (struct msg_request_t*)zrealloc(request, sizeof(*request) + request->data_length +
		      sizeof(data_len) + data_len);
	if (unlikely(!request)) {
		fprintf(stderr, "oom\n");
		return NULL;
	}

	struct msg_arg_t *arg;
	arg = (struct msg_arg_t *)(request->data + request->data_length);
	arg->size = data_len;
	memcpy(arg->data, data, data_len);

	request->data_length += sizeof(data_len) + data_len;

	return request;
}


msg_response_t *add_response_arg(msg_response_t *response, const void *data, uint32_t data_len)
{
	response = (struct msg_response_t*)zrealloc(response, sizeof(*response) + response->data_length +
		      sizeof(data_len) + data_len);
	if (unlikely(!response)) {
		fprintf(stderr, "oom\n");
		return NULL;
	}

	struct msg_arg_t *arg;
	arg = (struct msg_arg_t *)(response->data + response->data_length);
	arg->size = data_len;
	memcpy(arg->data, data, data_len);

	response->data_length += sizeof(data_len) + data_len;

	return response;
}

/* ==================== append_request_arg ==================== */   
/* Append data to the last argument.  This doesn't add a new argument */
/*void *append_message_arg(void *p, const void *data, uint32_t data_len)*/
/*{*/
	/*struct msg_header_t *header = (struct msg_header_t*)p;*/
	/*struct msg_arg_t *arg, *last_arg = NULL;*/

	/*if (header->data_length == 0)*/
		/*return add_message_arg(p, data, data_len);*/

	/*header = (struct msg_header_t*)zrealloc(header, sizeof(*header) + header->data_length + data_len);*/
	/*if (unlikely(!header)) {*/
		/*fprintf(stderr, "oom\n");*/
		/*return NULL;*/
	/*}*/

	/*for_each_arg(arg, header)*/
		/*last_arg = arg;*/

	/*memcpy(last_arg->data + last_arg->size, data, data_len);*/
	/*last_arg->size += data_len;*/

	/*header->data_length += data_len;*/

	/*return header;*/
/*}*/

