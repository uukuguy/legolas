/**
 * @file   legolas.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-04-30 23:00:00
 * 
 * @brief  
 * 
 * 
 */

#include "net.h"
#include <signal.h>

static void __attribute__((constructor)) net_init(void)
{
    signal(SIGPIPE, SIG_IGN);
}

