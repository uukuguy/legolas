/**
 * @file   daemon.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-04-30 22:07:28
 * 
 * @brief  
 * 
 * 
 */

#ifndef __COMMON_BASE_DAEMON__
#define __COMMON_BASE_DAEMON__

#include <stdlib.h>

typedef int(*DAEMON_LOOP)(void *data);

#ifdef __cplusplus
extern "C" {
#endif
    
    extern int daemon_fork(DAEMON_LOOP daemon_loop, void *data);
    extern int daemon_fork2(DAEMON_LOOP daemon_loop1, DAEMON_LOOP daemon_loop2, void *data);
    extern int daemon_fork3(DAEMON_LOOP daemon_loop1, DAEMON_LOOP daemon_loop2, DAEMON_LOOP daemon_loop3, void *data);
    extern int daemon_fork4(DAEMON_LOOP daemon_loop1, DAEMON_LOOP daemon_loop2, DAEMON_LOOP daemon_loop3, DAEMON_LOOP daemon_loop4, void *data);
    extern int daemon_forks(DAEMON_LOOP daemon_loops[], size_t n, void *data);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __COMMON_BASE_DAEMON__ */
