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

typedef struct logfile_info_t logfile_info_t;

logfile_info_t *logfile_open(int id, const char *log_root_path, int bWrite);
void logfile_close(logfile_info_t *logfile);
int logfile_write(logfile_info_t *logfile, const char *buf, uint32_t buf_size);
int logfile_read(logfile_info_t *logfile, const char *buf, uint32_t buf_size);

#endif /* __LOGFILE_H__ */

