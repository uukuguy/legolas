/**
 * @file   logfile.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-06-16 21:48:38
 * 
 * @brief  
 * 
 * 
 */

#include "logfile.h"
#include "filesystem.h"
#include "zmalloc.h"
#include "common.h"
#include "logger.h"

typedef struct logfile_info_t {
    uint32_t id;
    char logfile_name[NAME_MAX];
    uint32_t file_size;

    int file_handle;
} logfile_info_t;


/* ==================== logfile_open() ==================== */ 
logfile_info_t *logfile_open(int id, const char *log_root_path, int bWrite)
{
    logfile_info_t *logfile = (logfile_info_t*)zmalloc(sizeof(logfile_info_t));
    logfile->id = id;
    logfile->file_handle = -1;
    sprintf(logfile->logfile_name, "%s/legolas-%02d.log", log_root_path, id);
    if ( bWrite == 1 ) {
        if ( mkdir_if_not_exist(log_root_path) == 0 ){
            logfile->file_handle = open(logfile->logfile_name, O_CREAT | O_APPEND | O_DIRECT, 0640);
            if ( logfile->file_handle == -1 ) {
                error_log("Create logfile(%d) %s failed.", id, logfile->logfile_name);
                zfree(logfile);
                logfile = NULL;
            } else {
                info_log("Create logfile(%d): %s Success.", id, logfile->logfile_name);
            }
        } else {
            error_log("Create %s failed.", log_root_path);
            zfree(logfile);
            logfile = NULL;
        }
    } else {
        logfile->file_handle = open(logfile->logfile_name, O_RDONLY | O_DIRECT, 0640);
        if ( logfile->file_handle == -1 ) {
            error_log("Open logfile(%d) %s failed.", id, logfile->logfile_name);
            zfree(logfile);
            logfile = NULL;
        } else {
            info_log("Open logfile(%d): %s Success.", id, logfile->logfile_name);
        }
    }

    return logfile;
}

/* ==================== logfile_close() ==================== */ 
void logfile_close(logfile_info_t *logfile)
{
    if ( logfile->file_handle > 0 ){
        close(logfile->file_handle);
        logfile->file_handle = -1;
    }
}

/* ==================== logfile_write() ==================== */ 
int logfile_write(logfile_info_t *logfile, const char *buf, uint32_t buf_size)
{
    assert(buf != NULL && buf_size > 0);
    int ret = -1;
    if ( logfile->file_handle > 0 ){
        ret = write(logfile->file_handle, buf, buf_size);
        if ( ret < buf_size ){
            error_log("Write logfile(%d): %s failed. errno:%d ", logfile->id, logfile->logfile_name, errno);
        }
    }

    return ret;
}

/* ==================== logfile_read() ==================== */ 
int logfile_read(logfile_info_t *logfile, const char *buf, uint32_t buf_size)
{
    int readed = 0;
    if ( logfile->file_handle > 0 ){
        readed = read(logfile->file_handle, (void*)buf, buf_size);
        if ( readed < 0 ) {
            error_log("Read logfile(%d): %s failed. errno:%d", logfile->id, logfile->logfile_name, errno);
        }

    } else {
        readed = -1;
    }

    return readed;
}

