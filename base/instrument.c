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
#include "common.h"
#include "calltree.h"

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


static calltree_t *g_calltree = NULL;

/* ================ main_constructor() ================ */
void main_constructor( void )
{
}


/* ================ main_deconstructor() ================ */
void main_destructor( void )
{
    if ( g_calltree != NULL ){
        calltree_dump_to_file(g_calltree, "./calltree.log");
    }
}


/* ================ __cyg_profile_func_enter() ================ */
void __cyg_profile_func_enter( void *this, void *callsite )
{
    if ( g_calltree == NULL ){
        g_calltree = calltree_new();
    }
    calltree_enter_function(g_calltree, (uint64_t)this, (uint64_t)callsite);

}

/* ================ __cyg_profile_func_exit() ================ */
void __cyg_profile_func_exit( void *this, void *callsite )
{

    if ( g_calltree == NULL ){
        g_calltree = calltree_new();
    }
    calltree_exit_function(g_calltree, (uint64_t)this, (uint64_t)callsite);
}

