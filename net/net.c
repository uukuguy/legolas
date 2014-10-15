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

static void handle_pipe(int sig)
{
}

static void ignore_sigpipe_handle(void) 
{
    /*signal(SIGPIPE, SIG_IGN);*/
    struct sigaction sa;
    sa.sa_handler = SIG_IGN;
    sigaction(SIGPIPE, &sa, NULL);
}

__attribute__((unused)) 
static void restore_sigpipe_handle(void) 
{
    struct sigaction sa;
    sa.sa_handler = handle_pipe;
    sigemptyset(&sa.sa_mask);
    sigaction(SIGPIPE, &sa, NULL);
}

static void __attribute__((constructor)) net_init(void)
{
    ignore_sigpipe_handle();
}

