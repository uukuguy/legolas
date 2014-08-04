/**
 * @file   common.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-04-30 22:06:56
 * 
 * @brief  
 * 
 * 
 */

#ifndef __COMMON_COMMON_H__
#define __COMMON_COMMON_H__

#include <sys/stat.h> /* mkdir() */
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h> /* uint32_t */
#include <fcntl.h> /* O_DIRECT */
#include <unistd.h>
#include <errno.h>
#include <syslog.h>
#include <string.h>
#include <signal.h>
#include <sys/time.h> /* gettimeofday() struct timezone */
#include <time.h> /* struct tm, localtime() */
#include <pthread.h>
#include <sched.h> /* sched_yield() */
#include <limits.h> /* NAME_MAX */
#include <getopt.h>
#include <assert.h>
#include <uuid/uuid.h>
#include "pth.h"
#include "lthread.h"
#include "coro.h"

#ifndef UNUSED
#if defined(__clang__) ||                                \
    defined(__GNUC__) ||                                 \
defined(__INTEL_COMPILER) ||                         \
defined(__SUNPRO_C)
# define UNUSED __attribute__((unused))
#else
# define UNUSED
#endif
#endif

#endif /*  __COMMON_COMMON_H__ */

