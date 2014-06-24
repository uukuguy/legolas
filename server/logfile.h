/**
 * @file   logfile.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-06-16 21:48:01
 * 
 * @brief  
 * 
 * 
 */

#ifndef __LOGFILE_H__
#define __LOGFILE_H__

#include <stdint.h>
//#include "common.h"
#include <limits.h>

typedef struct logfile_t {
    uint32_t id;
    char logfile_name[NAME_MAX];
    uint32_t file_size;

    int file_handle;
} logfile_t;


logfile_t *logfile_new(uint32_t id, const char *logfile_name);
void logfile_free(logfile_t *logfile);

int logfile_open(logfile_t *logfile, int bWrite);
void logfile_close(logfile_t *logfile);
int logfile_write(logfile_t *logfile, const char *buf, uint32_t buf_size);
int logfile_read(logfile_t *logfile, const char *buf, uint32_t buf_size);

#endif /* __LOGFILE_H__ */

