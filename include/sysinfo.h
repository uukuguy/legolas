/**
 * @file   sysinfo.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-09-09 18:48:27
 * 
 * @brief  
 * 
 * 
 */

#ifndef __HOST_SYSINFO_H__
#define __HOST_SYSINFO_H__

#include "common.h"

typedef struct sysinfo_t {
    int num_processors;
    int page_size;
    int num_pages;
    int free_pages;
    int total_mem;
    int free_mem;
} sysinfo_t;

int retrieve_sysinfo(sysinfo_t *sysinfo);
void sysinfo_format(sysinfo_t *sysinfo, char *buf);
void log_sysinfo(void);

#endif /* __HOST_SYSINFO_H__ */

