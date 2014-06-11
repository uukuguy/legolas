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

#include <sys/stat.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <syslog.h>
#include <string.h>
#include <signal.h>
#include <sys/time.h>
#include <time.h>
#include <pthread.h>
#include <sched.h>
#include <limits.h>
#include <getopt.h>
#include <assert.h>

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

