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


#ifdef __cplusplus
extern "C" {
#endif

struct genc_binary_tree;
typedef struct function_t function_t;

/* ================ struct calltree_t ================ */
typedef struct calltree_t{
    struct genc_binary_tree *functions;
    uint32_t total_functions;
} calltree_t;

calltree_t *calltree_new(void) __attribute__ ((no_instrument_function));

void calltree_free(calltree_t *calltree) __attribute__ ((no_instrument_function));

function_t *calltree_find_function(calltree_t *calltree, uint64_t function_addr) __attribute__ ((no_instrument_function));

function_t *calltree_register_function_if_not_exist(calltree_t *calltree, uint64_t function_addr) __attribute__ ((no_instrument_function));

function_t *calltree_enter_function(calltree_t *calltree, uint64_t callee_addr, uint64_t caller_addr) __attribute__ ((no_instrument_function));

function_t *calltree_exit_function(calltree_t *calltree, uint64_t callee_addr, uint64_t caller_addr) __attribute__ ((no_instrument_function));

void calltree_dump_to_file(calltree_t *calltree, const char *filename) __attribute__ ((no_instrument_function));

#ifdef __cplusplus
}
#endif

#endif /* __CALLTREE_H__ */

