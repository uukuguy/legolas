/**
 * @file   function.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-10-10 08:45:27
 * 
 * @brief  
 * 
 * 
 */

#ifndef __FUNCTION_H__
#define __FUNCTION_H__

#include <stdint.h>
#include <time.h>
#include <sys/time.h>
#include "binary_tree.h"
//#include "slist.h"
#include "dlist.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct function_t function_t;

typedef struct function_finder_t {
    function_t *func;
    struct dlist_head dl_head;
} function_finder_t;

typedef struct name_by_address_t {
    uint64_t function_addr;
    char function_name[256];
    struct genc_bt_node_head bt_head;
} name_by_address_t;

/* ================ struct function_t ================ */
typedef struct function_t {
    uint64_t function_addr;
    char function_name[256];
    uint64_t total_called;
    uint64_t call_time;

    struct timeval start_time;
    struct timeval end_time;
    uint64_t from_functions_call_time;

    /* functions in calltree_t */
    struct genc_bt_node_head functions_by_name_bt_head;

    struct genc_bt_node_head name_by_address_bt_head;

    function_finder_t *from_functions;
    function_finder_t *to_functions;
    
    uint32_t from_functions_count;
    uint32_t to_functions_count;

} function_t;

function_finder_t *function_finder_new(function_t *func) __attribute__ ((no_instrument_function));

void function_finder_free(function_finder_t *function_finder) __attribute__ ((no_instrument_function));

function_t *function_new(uint64_t function_addr, const char *function_name, uint32_t name_len) __attribute__ ((no_instrument_function));

void function_free(function_t *function) __attribute__ ((no_instrument_function));

genc_bool_t function_address_less_cb(genc_bt_node_head_t* a, genc_bt_node_head_t* b, void* opaque) __attribute__ ((no_instrument_function));

genc_bool_t function_name_less_cb(genc_bt_node_head_t* a, genc_bt_node_head_t* b, void* opaque) __attribute__ ((no_instrument_function));

int function_find_from_function(function_t *function, function_t *from_function) __attribute__ ((no_instrument_function));

int function_find_to_function(function_t *function, function_t *to_function) __attribute__ ((no_instrument_function));

int function_add_from_function(function_t *function, function_t *from_function) __attribute__ ((no_instrument_function));

int function_add_from_function_if_not_exist(function_t *function, function_t *from_function) __attribute__ ((no_instrument_function));

int function_add_to_function(function_t *function, function_t *to_function) __attribute__ ((no_instrument_function));

int function_add_to_function_if_not_exist(function_t *function, function_t *to_function) __attribute__ ((no_instrument_function));

int get_function_name_from_address(uint64_t function_addr, char *function_name) __attribute__ ((no_instrument_function));

void function_get_function_name_from_address(function_t *function) __attribute__ ((no_instrument_function));

#ifdef __cplusplus
}
#endif

#endif /* __FUNCTION_H__ */

