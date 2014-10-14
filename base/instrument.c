/********************************************************************
 * File: instrument.c
 *
 * Instrumentation source -- link this with your application, and
 *  then execute to build trace data file (trace.txt).
 *
 * Author: M. Tim Jones <mtj@mtjones.com>
 *
 */

#include <stdio.h>
#include <stdlib.h>

#include <stdint.h>
#include <stdio.h>
#include <malloc.h>
#include <sys/time.h>
#include <pthread.h>
#include "common.h"
#include "calltree.h"
#include "function.h"
#include "dlist.h"

typedef struct calltree_t calltree_t;
typedef struct function_t function_t;


/* Function prototypes with attributes */
void main_constructor( void )
	__attribute__ ((no_instrument_function, constructor));

void main_destructor( void )
	__attribute__ ((no_instrument_function, destructor));

void __cyg_profile_func_enter( void *, void * ) 
	__attribute__ ((no_instrument_function));

void __cyg_profile_func_exit( void *, void * )
	__attribute__ ((no_instrument_function));


void create_calltree(void)
	__attribute__ ((no_instrument_function));

static calltree_t *g_calltree = NULL;
static uint64_t g_total_functions = 0;
static pthread_mutex_t g_calltree_lock;


void create_calltree(void)
{
    if ( g_calltree == NULL ){
        g_calltree = calltree_new();
        gettimeofday(&g_calltree->start_time, NULL);
        pthread_mutex_init(&g_calltree_lock, NULL);
    }

}
/* ================ main_constructor() ================ */
void main_constructor( void )
{
}


/* ================ main_deconstructor() ================ */
void main_destructor( void )
{
    pthread_mutex_destroy(&g_calltree_lock);

    if ( g_calltree != NULL ){
        gettimeofday(&g_calltree->end_time, NULL);
        printf("\n=-= Start to dump =-=\n");
        /*int file = open("./call_infos.log", O_CREAT | O_TRUNC | O_WRONLY, 0640);*/

        uint64_t idx = 0;
        struct dlist_head *call_infos = &g_calltree->call_infos->dl_head;
        genc_dlist_for_each_object(call_info_t, dl_head, call_info, call_infos) {
            idx++;
            if ( idx % 100 == 0 || (idx + 5 >= g_total_functions) )
            printf("Processing %zu/%zu\n", idx, g_total_functions);

            /*char caller_name[256];*/
            /*get_function_name_from_address(call_info->caller_addr, caller_name);*/
            /*char callee_name[256];*/
            /*get_function_name_from_address(call_info->callee_addr, callee_name);*/

            /*char buf[1024];*/
            /*sprintf(buf, "%c %s(0x%x) %s(0x%x) %zu\n", call_info->call_type, caller_name, call_info->caller_addr, callee_name, call_info->callee_addr, call_info->call_timestamp);*/
            /*write(file, buf, strlen(buf));*/

            if ( call_info->call_type == 'E' ){
                calltree_enter_function(g_calltree, call_info->callee_addr, call_info->caller_addr, call_info->call_timestamp);
            } else if ( call_info->call_type == 'X' ){
                calltree_exit_function(g_calltree, call_info->callee_addr, call_info->caller_addr, call_info->call_timestamp);
            }
        }
        /*close(file);*/
        printf("\n=-= calltree_dump_to_file =-=\n");
        calltree_dump_to_file(g_calltree, "./calltree");
    }
}


/* ================ __cyg_profile_func_enter() ================ */
void __cyg_profile_func_enter( void *this, void *callsite )
{
    create_calltree();

    call_info_t *call_info =  call_info_new('E', (uint64_t)callsite, (uint64_t)this, 0);
    pthread_mutex_lock(&g_calltree_lock);
    genc_dlist_insert_before(&call_info->dl_head, &g_calltree->call_infos->dl_head);
    __sync_add_and_fetch(&g_total_functions, 1);
    pthread_mutex_unlock(&g_calltree_lock);
}

/* ================ __cyg_profile_func_exit() ================ */
void __cyg_profile_func_exit( void *this, void *callsite )
{
    create_calltree();

    call_info_t *call_info =  call_info_new('X', (uint64_t)callsite, (uint64_t)this, 0);
    pthread_mutex_lock(&g_calltree_lock);
    genc_dlist_insert_before(&call_info->dl_head, &g_calltree->call_infos->dl_head);
    __sync_add_and_fetch(&g_total_functions, 1);
    pthread_mutex_unlock(&g_calltree_lock);
}

