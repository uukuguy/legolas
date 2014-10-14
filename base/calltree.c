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
#include <sys/time.h>
#include "common.h"
#include "calltree.h"
#include "function.h"

uint64_t calc_call_time(struct timeval *start_time, struct timeval *end_time)
{
    uint64_t sec = end_time->tv_sec - start_time->tv_sec;
    uint64_t usec = 0;
    if ( end_time->tv_usec >= start_time->tv_usec ){
        usec = end_time->tv_usec - start_time->tv_usec;
    } else {
        sec--;
        usec = end_time->tv_usec + 1000000 - start_time->tv_usec;
    }
    uint64_t call_time = sec * 1000000 + usec;

    return call_time;
}

call_info_t *call_info_new(uint8_t call_type, uint64_t caller_addr, uint64_t callee_addr, uint64_t call_timestamp)
{
    call_info_t *call_info = (call_info_t*)malloc(sizeof(call_info_t));
    memset(call_info, 0, sizeof(call_info_t));
    call_info->call_type = call_type;
    call_info->caller_addr = caller_addr;
    call_info->callee_addr = callee_addr;
    call_info->call_timestamp = call_timestamp;

    genc_dlist_init(&call_info->dl_head);

    return call_info;
}

void call_info_free(call_info_t *call_info)
{
    free(call_info);
}

/* ================ calltree_new() ================ */
calltree_t *calltree_new(void) 
{
    calltree_t *calltree = (calltree_t*)malloc(sizeof(calltree_t));
    memset(calltree, 0, sizeof(calltree_t));

    calltree->functions_by_name = (struct genc_binary_tree*)malloc(sizeof(struct genc_binary_tree));
    memset(calltree->functions_by_name, 0, sizeof(struct genc_binary_tree));
    genc_binary_tree_init(calltree->functions_by_name, function_name_less_cb, calltree);

    calltree->name_by_address = (struct genc_binary_tree*)malloc(sizeof(struct genc_binary_tree));
    memset(calltree->name_by_address, 0, sizeof(struct genc_binary_tree));
    genc_binary_tree_init(calltree->name_by_address, function_address_less_cb, calltree);


    calltree->call_infos = call_info_new('S', 0, 0, 0);

    return calltree;
}

/* ================ calltree_free() ================ */
void calltree_free(calltree_t *calltree) 
{
    call_info_free(calltree->call_infos);
    calltree->call_infos = NULL;

    free(calltree->functions_by_name);
    free(calltree);
}

/* ================ calltree_find_function_by_name() ================ */
function_t *calltree_find_function_by_name(calltree_t *calltree, const char *function_name) 
{
    function_t dummy_function;
    strcpy(dummy_function.function_name, function_name);

    function_t *function = NULL; 

    genc_bt_node_head_t *node = genc_bt_find(calltree->functions_by_name, &dummy_function.functions_by_name_bt_head);
    if ( node != NULL ){
        function = genc_container_of_notnull(node, function_t, functions_by_name_bt_head);
    }
    return function;
}

function_t *calltree_register_function_if_not_exist(calltree_t *calltree, uint64_t function_addr, const char *function_name) 
{

    function_t *function = calltree_find_function_by_name(calltree, function_name);

    if ( function == NULL ){
        uint32_t name_len = strlen(function_name);

        function = function_new(function_addr, function_name, name_len);

        genc_bt_insert(calltree->functions_by_name, &function->functions_by_name_bt_head);
        __sync_add_and_fetch(&calltree->total_functions, 1);
    }

    return function;
}

void calltree_search_function_name_by_address(calltree_t *calltree, uint64_t function_addr, char *function_name) __attribute__ ((no_instrument_function));

void calltree_search_function_name_by_address(calltree_t *calltree, uint64_t function_addr, char *function_name)
{
    name_by_address_t nba;
    nba.function_addr = function_addr;

    name_by_address_t *nba0;
    genc_bt_node_head_t *node = genc_bt_find(calltree->name_by_address, &nba.bt_head);
    if ( node != NULL ){
        nba0 = genc_container_of_notnull(node, name_by_address_t, bt_head);
        strcpy(function_name, nba0->function_name);
    } else {
        get_function_name_from_address(function_addr, function_name);

        nba0 = (name_by_address_t*)malloc(sizeof(name_by_address_t));
        memset(nba0, 0, sizeof(name_by_address_t));
        nba0->function_addr = function_addr;
        strcpy(nba0->function_name, function_name);

        genc_bt_insert(calltree->name_by_address, &nba0->bt_head);
    }
}

/* ================ calltree_enter_function() ================ */
function_t *calltree_enter_function(calltree_t *calltree, uint64_t callee_addr, uint64_t caller_addr, uint64_t call_timestamp)
{

    char caller_name[256];
    calltree_search_function_name_by_address(calltree, caller_addr, caller_name);

    char callee_name[256];
    calltree_search_function_name_by_address(calltree, callee_addr, callee_name);


    function_t *caller = calltree_register_function_if_not_exist(calltree, caller_addr,  caller_name);
    function_t *callee = calltree_register_function_if_not_exist(calltree, callee_addr,  callee_name);
    
    assert(callee != NULL);
    assert(caller != NULL);

    function_add_to_function_if_not_exist(caller, callee);
    function_add_from_function_if_not_exist(callee, caller);

    __sync_add_and_fetch(&callee->total_called, 1);

    struct timeval start_time;
    gettimeofday(&start_time, NULL);
    /* FIXME 2014-10-10 01:12:42 */
    start_time.tv_sec -= calltree->start_time.tv_sec;

    callee->start_time.tv_sec += start_time.tv_sec;
    uint64_t usec = callee->start_time.tv_usec + start_time.tv_usec;
    if ( usec > 1000000 ){
        callee->start_time.tv_sec++;
        callee->start_time.tv_usec = usec - 1000000;
    } else {
        callee->start_time.tv_usec = usec;
    }

    return callee;
}

function_t *calltree_exit_function(calltree_t *calltree, uint64_t callee_addr, uint64_t caller_addr, uint64_t call_timestamp)
{
    char callee_name[256];
    calltree_search_function_name_by_address(calltree, callee_addr, callee_name);

    function_t *callee = calltree_find_function_by_name(calltree, callee_name);
    assert(callee != NULL);
    if ( callee != NULL ){

        struct timeval end_time;
        gettimeofday(&end_time, NULL);
        /* FIXME 2014-10-10 01:12:42 */
        end_time.tv_sec -= calltree->start_time.tv_sec;

        callee->end_time.tv_sec += end_time.tv_sec;
        uint64_t usec = callee->end_time.tv_usec + end_time.tv_usec;
        if ( usec > 1000000 ){
            callee->end_time.tv_sec++;
            callee->end_time.tv_usec = usec - 1000000;
        } else {
            callee->end_time.tv_usec = usec;
        }
    }

    return callee;
}

void format_call_time(uint64_t call_time, char *sz_call_time) __attribute__ ((no_instrument_function));
void format_call_time(uint64_t call_time, char *sz_call_time)
{
    if ( call_time < 1000 ){
        sprintf(sz_call_time, "%zu us", call_time);
    } else if ( call_time < 1000000 ) {
        sprintf(sz_call_time, "%zu ms", call_time / 1000);
    } else {
        uint64_t sec = call_time / 1000000;
        uint64_t ms = (call_time - sec * 1000000 ) / 1000;
        sprintf(sz_call_time, "%zu.%03zu sec", sec, ms);
    }
}

void calltree_dump_to_file(calltree_t *calltree, const char *filename)
{
    uint64_t calltree_call_time = calc_call_time(&calltree->start_time, &calltree->end_time);

    char dotfile_name[256];
    sprintf(dotfile_name, "%s.dot", filename);

    int dotfile = open(dotfile_name, O_CREAT | O_TRUNC | O_WRONLY, 0640);

    const char *graphic_name = "calltree";

    char dotbuf[1024];
    sprintf(dotbuf, "digraph %s {\n\n", graphic_name);

    write(dotfile, dotbuf, strlen(dotbuf));

    char logfile_name[256];
    sprintf(logfile_name, "%s.log", filename);
    int file = open(logfile_name, O_CREAT | O_TRUNC | O_WRONLY, 0640);
    
    char buf[1024];

    char sz_calltree_call_time[256];
    format_call_time(calltree_call_time, sz_calltree_call_time);

    sprintf(dotbuf, "CallTree [shape=polygon, sides=5, peripheries=3, color=lightblue, style=filled, label=\"CallTree\n %s\"]", sz_calltree_call_time);
    write(dotfile, dotbuf, strlen(dotbuf));

    genc_bt_node_head_t* item = genc_bt_last_item(calltree->functions_by_name); 
    while ( item != NULL ){
        function_t *function = genc_container_of_notnull(item, function_t, functions_by_name_bt_head);

        function->call_time = calc_call_time(&function->start_time, &function->end_time);

        item = genc_bt_prev_item(calltree->functions_by_name, item);
    }

    item = genc_bt_last_item(calltree->functions_by_name); 
    while ( item != NULL ){
        function_t *function = genc_container_of_notnull(item, function_t, functions_by_name_bt_head);

        uint64_t from_functions_call_time = 0;
        struct dlist_head *functions = &function->from_functions->dl_head;
        genc_dlist_for_each_object(function_finder_t, dl_head, function_finder, functions) {
            function_t *func = function_finder->func;
            from_functions_call_time += func->call_time;
        }
        function->from_functions_call_time = from_functions_call_time;

        item = genc_bt_prev_item(calltree->functions_by_name, item);
    }

    uint32_t idx;
    item = genc_bt_last_item(calltree->functions_by_name); 
    while ( item != NULL ){
        function_t *function = genc_container_of_notnull(item, function_t, functions_by_name_bt_head);

        const char *Shape = "shape=box";
        const char *DotColor = "";
        /*if ( function->from_functions_count == 1 ){*/

            /*struct dlist_head *functions = &function->from_functions->dl_head;*/
            /*function_finder_t *parent = genc_dlist_last_object(functions, function_finder_t, dl_head); */
            /*assert(parent != NULL);*/
            /*function_t *parent_function = parent->func;*/

            /*if ( function->call_time >=  parent_function->call_time / 2 ){*/
                /*DotColor = "color=Red";*/
            /*}*/
        /*} else if ( function->from_functions_count > 1 ) {*/
        int is_heavy_call = 0;
        if ( function->from_functions_count >= 1 ) {
            if ( function->call_time >= function->from_functions_call_time / 3 ){
                /*DotColor = "color=\".75 .08 .13\" style=filled fontcolor=white";*/
                is_heavy_call = 1;

                if ( function->call_time > 0.02 * calltree_call_time ){
                    Shape = "shape=box";
                    DotColor = "color=Red style=bold fontcolor=Red";
                }
            }
        } else {
            Shape = "shape=ellipse color=Green style=bold";
        }

        char sz_call_time[256];
        format_call_time(function->call_time, sz_call_time);
        double p = (double)function->call_time / (double)calltree_call_time;

        sprintf(dotbuf, "  %s [%s label=\"%s (%zux %s %.2f%%)\" %s]\n", function->function_name, Shape, function->function_name, function->total_called, sz_call_time, p * 100, DotColor);
        write(dotfile, dotbuf, strlen(dotbuf));

        sprintf(buf, "%s(%p): %zux %s %.2f%%\n", function->function_name, (void*)function->function_addr, function->total_called, sz_call_time, p * 100); 
        write(file, buf, strlen(buf));  

        /* ---------- from functions ---------- */
        if ( 1 )
        {
            struct dlist_head *functions = &function->from_functions->dl_head;

            uint32_t from_functions_count = function->from_functions_count;
            char sz_count[16];
            sprintf(sz_count, "    (%d) ", from_functions_count);
            write(file, sz_count, strlen(sz_count));
            
            write(file, "  from: ", 8);
            genc_dlist_for_each_object(function_finder_t, dl_head, function_finder, functions) {
                function_t *func = function_finder->func;
                if ( func != NULL ){
                    char szItem[256];
                    sprintf(szItem, "%s(%p) ", func->function_name, (void*)func->function_addr);
                    write(file, szItem, strlen(szItem));

                    if ( is_heavy_call == 1 ) {
                        sprintf( dotbuf, "  %s -> %s [color=Red]\n", func->function_name, function->function_name); 
                    } else {
                        sprintf( dotbuf, "  %s -> %s \n", func->function_name, function->function_name); 
                    }

                    write(dotfile, dotbuf, strlen(dotbuf));
                }
            }
            write(file, "\n", 1);
        }


        /* ---------- to functions ---------- */
        if ( 1)
        {
            struct dlist_head *functions = &function->to_functions->dl_head;

            uint32_t to_functions_count = function->to_functions_count;
            char sz_count[16];
            sprintf(sz_count, "    (%d) ", to_functions_count);
            write(file, sz_count, strlen(sz_count));

            write(file, "    to: ", 8);
            genc_dlist_for_each_object(function_finder_t, dl_head, function_finder, functions) {
                function_t *func = function_finder->func;
                char szItem[256];
                sprintf(szItem, "%s(%p) ", func->function_name, (void*)func->function_addr);
                write(file, szItem, strlen(szItem));

                /*sprintf( dotbuf, "  %s -> %s \n", function->function_name, func->function_name); */
                /*write(dotfile, dotbuf, strlen(dotbuf));*/
            }
            write(file, "\n", 1);
        }

        item = genc_bt_prev_item(calltree->functions_by_name, item);
        idx++;
    }

    close(file);

    sprintf( dotbuf, "\n}\n");
    write(dotfile, dotbuf, strlen(dotbuf));

    close(dotfile);
}

