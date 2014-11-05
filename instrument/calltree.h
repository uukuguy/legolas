/**
 * @file   calltree.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-10-10 08:47:32
 * 
 * @brief  
 * 
 * 
 */

#ifndef __CALLTREE_H__
#define __CALLTREE_H__

#include <stdint.h>
#include <time.h>
#include "dlist.h"

#ifdef __cplusplus
extern "C" {
#endif

struct genc_binary_tree;
typedef struct function_t function_t;

typedef struct call_info_t {
    char call_type;
    uint64_t caller_addr;
    uint64_t callee_addr;
    uint64_t call_timestamp;

    struct dlist_head dl_head;
} call_info_t;

call_info_t *call_info_new(uint8_t call_type, uint64_t caller_addr, uint64_t callee_addr, uint64_t call_timestamp) __attribute__ ((no_instrument_function));

void call_info_free(call_info_t *call_info) __attribute__ ((no_instrument_function));

/* ================ struct calltree_t ================ */
typedef struct calltree_t{
    struct genc_binary_tree *functions_by_name;
    struct call_info_t *call_infos;

    uint32_t total_functions;

    struct timeval start_time;
    struct timeval end_time;


    struct genc_binary_tree *name_by_address;
} calltree_t;

calltree_t *calltree_new(void) __attribute__ ((no_instrument_function));

void calltree_free(calltree_t *calltree) __attribute__ ((no_instrument_function));

uint64_t calc_call_time(struct timeval *start_time, struct timeval *end_time) __attribute__ ((no_instrument_function));

//function_t *calltree_find_function(calltree_t *calltree, uint64_t function_addr) __attribute__ ((no_instrument_function));
//function_t *calltree_register_function_if_not_exist(calltree_t *calltree, uint64_t function_addr) __attribute__ ((no_instrument_function));
//
function_t *calltree_find_function_by_name(calltree_t *calltree, const char *function_name) __attribute__ ((no_instrument_function));

function_t *calltree_register_function_if_not_exist(calltree_t *calltree, uint64_t function_addr, const char *function_name) __attribute__ ((no_instrument_function));

function_t *calltree_enter_function(calltree_t *calltree, uint64_t callee_addr, uint64_t caller_addr, uint64_t call_timestamp) __attribute__ ((no_instrument_function));

function_t *calltree_exit_function(calltree_t *calltree, uint64_t callee_addr, uint64_t caller_addr, uint64_t call_timestamp) __attribute__ ((no_instrument_function));

void calltree_dump_to_file(calltree_t *calltree, const char *filename) __attribute__ ((no_instrument_function));

#ifdef __cplusplus
}
#endif

#endif /* __CALLTREE_H__ */

