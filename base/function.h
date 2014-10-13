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
#include "binary_tree.h"
//#include "slist.h"
#include "dlist.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ================ struct function_t ================ */
typedef struct function_t {
    uint64_t function_addr;
    char function_name[256];
    uint64_t total_called;
    uint64_t call_time;

    /* functions in calltree_t */
    struct genc_bt_node_head functions_bt_head;

    //struct slist_head from_functions_list_head;
    //struct slist_head to_functions_list_head;
    struct dlist_head from_functions_list_head;
    struct dlist_head to_functions_list_head;
    uint32_t from_functions_count;
    uint32_t to_functions_count;

} function_t;


function_t *function_new(uint64_t function_addr, const char *function_name, uint32_t name_len) __attribute__ ((no_instrument_function));

void function_free(function_t *function) __attribute__ ((no_instrument_function));

genc_bool_t function_address_less_cb(genc_bt_node_head_t* a, genc_bt_node_head_t* b, void* opaque) __attribute__ ((no_instrument_function));

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

