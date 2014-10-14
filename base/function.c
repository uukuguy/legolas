/**
 * @file   function.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-10-10 08:48:57
 * 
 * @brief  
 * 
 * 
 */

#include <malloc.h>
#include <memory.h>
#include "function.h"

function_finder_t *function_finder_new(function_t *func) 
{
    function_finder_t *function_finder = (function_finder_t*)malloc(sizeof(function_finder_t));
    memset(function_finder, 0, sizeof(function_finder_t));
    function_finder->func = func;
    genc_dlist_init(&function_finder->dl_head);
    return function_finder;
}

void function_finder_free(function_finder_t *function_finder)
{
    free(function_finder);
}

/* ================ function_new() ================ */
function_t *function_new(uint64_t function_addr, const char *function_name, uint32_t name_len) 
{
    function_t *function = (function_t*)malloc(sizeof(function_t));
    memset(function, 0, sizeof(function_t));

    function->function_addr = function_addr;

    function->from_functions = function_finder_new(NULL);
    function->to_functions = function_finder_new(NULL);

    if ( name_len > 0 ){
        uint32_t len = name_len < 128 ? name_len : 127;
        memcpy(function->function_name, function_name, len);
        function->function_name[len] = '\0';
    } else {
        function->function_name[0] = '(';
        function->function_name[1] = 'n';
        function->function_name[2] = 'u';
        function->function_name[3] = 'l';
        function->function_name[4] = 'l';
        function->function_name[5] = ')';
        function->function_name[6] = '\0';
    }

    return function;
}

/* ================ function_free() ================ */
void function_free(function_t *function) 
{
    function_finder_free(function->from_functions);
    function_finder_free(function->to_functions);
    free(function);
}

/* ================ function_address_less_cb() ================ */
genc_bool_t function_address_less_cb(genc_bt_node_head_t* a, genc_bt_node_head_t* b, void* opaque) 
{

    name_by_address_t *func_a = genc_container_of_notnull(a, name_by_address_t, bt_head);
    name_by_address_t *func_b = genc_container_of_notnull(b, name_by_address_t, bt_head);

    return func_a->function_addr < func_b->function_addr;
}

genc_bool_t function_name_less_cb(genc_bt_node_head_t* a, genc_bt_node_head_t* b, void* opaque) 
{

    function_t *func_a = genc_container_of_notnull(a, function_t, functions_by_name_bt_head);
    function_t *func_b = genc_container_of_notnull(b, function_t, functions_by_name_bt_head);

    /*return func_a->function_addr < func_b->function_addr;*/
    return strcmp(func_a->function_name, func_b->function_name) < 0;
}
/* ================ function_find_from_function() ================ */
int function_find_from_function(function_t *function, function_t *from_function) 
{
    uint32_t idx = 0;

    struct dlist_head *functions = &function->from_functions->dl_head;
    genc_dlist_for_each_object(function_finder_t, dl_head, function_finder, functions) {
        if ( function_finder->func == from_function ){
            return idx;
        }
        idx++;
    }

    return -1;
}

/* ================ function_find_to_function() ================ */
int function_find_to_function(function_t *function, function_t *to_function)
{
    uint32_t idx = 0;

    struct dlist_head *functions = &function->to_functions->dl_head;
    genc_dlist_for_each_object(function_finder_t, dl_head, function_finder, functions) {
        if ( function_finder->func == to_function ){
            return idx;
        }
        idx++;
    }

    return -1;
}


/* ================ function_add_from_function() ================ */
int function_add_from_function(function_t *function, function_t *from_function)
{
    struct dlist_head *pos = &function->from_functions->dl_head;

    function_finder_t *function_finder = function_finder_new(from_function);
    genc_dlist_insert_after(&function_finder->dl_head, pos);
    __sync_add_and_fetch(&function->from_functions_count, 1);

    return 0;
}

/* ================ function_add_from_function_if_not_exist() ================ */
int function_add_from_function_if_not_exist(function_t *function, function_t *from_function) 
{
    if ( function_find_from_function(function, from_function) < 0 ) {
        return function_add_from_function(function, from_function);
    } else
        return -1; 
}

/* ================ function_add_to_function() ================ */
int function_add_to_function(function_t *function, function_t *to_function)
{
    struct dlist_head *pos = &function->to_functions->dl_head;

    function_finder_t *function_finder = function_finder_new(to_function);
    genc_dlist_insert_before(&function_finder->dl_head, pos);
    __sync_add_and_fetch(&function->to_functions_count, 1);

    return 0;
}

/* ================ function_add_to_function_if_not_exist() ================ */
int function_add_to_function_if_not_exist(function_t *function, function_t *to_function) 
{
    if ( function_find_to_function(function, to_function) < 0 ) {
        return function_add_to_function(function, to_function);
    } else
        return -1;
}

/* ================ get_function_name_from_address() ================ */
int get_function_name_from_address(uint64_t function_addr, char *function_name)
{
    char command[256];
    sprintf(command, "/usr/bin/addr2line -e ./bin/legolas -f -s -p %p", (void*)function_addr);

    FILE *p = popen(command, "r");
    int len = fread(function_name, 1, 1023, p);
    pclose(p);

    if ( len > 0 ){
        int n;
        for ( n = 0 ; n < len ; n++ ){
            if ( function_name[n] == ' ' || function_name[n] == '.'){
                function_name[n] = '\0';
                len = n;
                break;
            }
        }
    }

    return len;
}

/* ================ function_get_function_name_from_address() ================ */
void function_get_function_name_from_address(function_t *function)
{
    char function_name[256];
    int len = get_function_name_from_address(function->function_addr, function_name);
    if ( len > 0 )
        memcpy(function->function_name, function_name, len);
    function->function_name[len] = '\0';
}

