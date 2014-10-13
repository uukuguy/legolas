/**
 * @file   calltree.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-10-10 08:53:17
 * 
 * @brief  
 * 
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <memory.h>
#include <assert.h>
#include "common.h"
#include "calltree.h"
#include "function.h"

/* ================ calltree_new() ================ */
calltree_t *calltree_new(void) 
{
    calltree_t *calltree = (calltree_t*)malloc(sizeof(calltree_t));
    memset(calltree, 0, sizeof(calltree_t));

    calltree->functions = (struct genc_binary_tree*)malloc(sizeof(struct genc_binary_tree));
    memset(calltree->functions, 0, sizeof(struct genc_binary_tree));

    genc_binary_tree_init(calltree->functions, function_address_less_cb, calltree);

    return calltree;
}

/* ================ calltree_free() ================ */
void calltree_free(calltree_t *calltree) 
{
    free(calltree->functions);
    free(calltree);
}

/* ================ calltree_find_function() ================ */
function_t *calltree_find_function(calltree_t *calltree, uint64_t function_addr) 
{
    function_t dummy_function;
    dummy_function.function_addr = function_addr;

    function_t *function = NULL; 

    genc_bt_node_head_t *node = genc_bt_find(calltree->functions, &dummy_function.functions_bt_head);
    if ( node != NULL ){
        function = genc_container_of_notnull(node, function_t, functions_bt_head);
    }
    return function;
}

function_t *calltree_register_function_if_not_exist(calltree_t *calltree, uint64_t function_addr) 
{
    function_t *function = calltree_find_function(calltree, function_addr);
    if ( function == NULL ){
        const char *function_name = NULL;
        uint32_t name_len = 0;

        function = function_new(function_addr, function_name, name_len);
        function_get_function_name_from_address(function);

        genc_bt_insert(calltree->functions, &function->functions_bt_head);
        __sync_add_and_fetch(&calltree->total_functions, 1);
    }

    return function;
}

/* ================ calltree_enter_function() ================ */
function_t *calltree_enter_function(calltree_t *calltree, uint64_t callee_addr, uint64_t caller_addr)
{
    function_t *caller = calltree_register_function_if_not_exist(calltree, caller_addr);
    function_t *callee = calltree_register_function_if_not_exist(calltree, callee_addr);
    
    assert(callee != NULL);
    assert(caller != NULL);

    function_add_to_function_if_not_exist(caller, callee);
    function_add_from_function_if_not_exist(callee, caller);

    __sync_add_and_fetch(&callee->total_called, 1);

    return callee;
}

function_t *calltree_exit_function(calltree_t *calltree, uint64_t callee_addr, uint64_t caller_addr)
{
    function_t *caller = calltree_find_function(calltree, caller_addr);
    /*function_t *callee = calltree_find_function(calltree, callee_addr);*/

    return caller;
}

void calltree_dump_to_file(calltree_t *calltree, const char *filename)
{
    int file = open(filename, O_CREAT | O_TRUNC | O_WRONLY, 0640);
    
    char buf[1024];

    genc_bt_node_head_t* item = genc_bt_last_item(calltree->functions); 
    while ( item != NULL ){
        function_t *function = genc_container_of_notnull(item, function_t, functions_bt_head);

        /*function_get_function_name_from_address(function);*/

        sprintf(buf, "%s(%p): %zu %zu\n", function->function_name, (void*)function->function_addr, function->total_called, function->call_time); 
        write(file, buf, strlen(buf));  

        /* ---------- from functions ---------- */
        if ( 1 )
        {
            struct dlist_head *functions = &function->from_functions_list_head;

            uint32_t from_functions_count = function->from_functions_count;
            char sz_count[16];
            sprintf(sz_count, "    (%d) ", from_functions_count);
            write(file, sz_count, strlen(sz_count));
            
            genc_dlist_for_each_object(function_t, from_functions_list_head, func, functions) {
                char szItem[256];
                sprintf(szItem, "from: %s(%p) ", func->function_name, (void*)func->function_addr);
                write(file, szItem, strlen(szItem));
            }
            write(file, "\n", 1);
        }


        /* ---------- to functions ---------- */
        if ( 1)
        {
            struct dlist_head *functions = &function->to_functions_list_head;

            uint32_t to_functions_count = function->to_functions_count;
            char sz_count[16];
            sprintf(sz_count, "    (%d) ", to_functions_count);
            write(file, sz_count, strlen(sz_count));

            genc_dlist_for_each_object(function_t, to_functions_list_head, func, functions) {
                char szItem[256];
                sprintf(szItem, "  to: %s(%p) ", func->function_name, (void*)func->function_addr);
                write(file, szItem, strlen(szItem));
            }
            write(file, "\n", 1);
        }

        item = genc_bt_prev_item(calltree->functions, item);
    }

    close(file);
}

