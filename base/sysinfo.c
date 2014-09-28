/**
 * @file   sysinfo.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-09-09 18:48:27
 * 
 * @brief  
 * 
 * 
 */

/*#include "common.h"*/
/*#include "host_sysinfo.h"*/
/*#include <memory.h>*/
#include "logger.h"
#include "sysinfo.h"


int retrive_sysinfo(sysinfo_t *sysinfo)
{
    memset(sysinfo, 0, sizeof(sysinfo_t));

    sysinfo->num_processors = sysconf(_SC_NPROCESSORS_CONF);
    sysinfo->page_size = sysconf(_SC_PAGESIZE);
    sysinfo->num_pages = sysconf(_SC_PHYS_PAGES);
    sysinfo->free_pages = sysconf(_SC_AVPHYS_PAGES);
    long total_mem = (long)sysinfo->page_size * (long)sysinfo->num_pages;
    sysinfo->total_mem = total_mem / (1024 * 1024);
    long free_mem = (long)sysinfo->page_size * (long)sysinfo->free_pages;
    sysinfo->free_mem = free_mem / (1024 * 1024);

    return 0;
}

void sysinfo_format(sysinfo_t *sysinfo, char *buf)
{
    sprintf(buf, "Processors: %d Page Size: %d Total Pages: %d Free Pages: %d Total Mem: %dMB Free Mem: %dMB",
            sysinfo->num_processors,
            sysinfo->page_size,
            sysinfo->num_pages,
            sysinfo->free_pages,
            sysinfo->total_mem,
            sysinfo->free_mem
           );
}

void log_sysinfo(void)
{
    sysinfo_t sysinfo;
    retrive_sysinfo(&sysinfo);
    char szSysinfo[1024];
    sysinfo_format(&sysinfo, szSysinfo);

    info_log("\n---------- %s ----------\n", szSysinfo);
}


