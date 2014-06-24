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
#include "common.h"
#include "logger.h"
#include "zmalloc.h"
#include "logger.h"

logfile_t *logfile_new(uint32_t id, const char *logfile_name)
{
    logfile_t *logfile = (logfile_t *)zmalloc(sizeof(logfile_t));

    logfile->id = id;

    uint32_t len = strlen(logfile_name);
    memcpy(logfile->logfile_name, logfile_name, len);
    logfile->logfile_name[len] = '\0';

    return logfile;
}

void logfile_free(logfile_t *logfile)
{
    zfree(logfile);
}

/* ==================== logfile_open() ==================== */ 
int logfile_open(logfile_t *logfile, int bWrite)
{
    logfile->file_handle = -1;
    if ( bWrite == 1 ) {
        logfile->file_handle = open(logfile->logfile_name, O_CREAT | O_APPEND, 0640);
        if ( logfile->file_handle == -1 ) {
            error_log("Create logfile(%d) %s failed.", logfile->id, logfile->logfile_name);
            return -1;
        } else {
            logfile->file_size = lseek(logfile->file_handle, 0, SEEK_CUR);
            info_log("Create logfile(%d): %s Success.", logfile->id, logfile->logfile_name);
        }
    } else {
        logfile->file_handle = open(logfile->logfile_name, O_RDONLY, 0640);
        if ( logfile->file_handle == -1 ) {
            error_log("Open logfile(%d) %s failed.", logfile->id, logfile->logfile_name);
            return -1;
        } else {
            logfile->file_size = lseek(logfile->file_handle, 0, SEEK_CUR);
            info_log("Open logfile(%d): %s Success.", logfile->id, logfile->logfile_name);
        }
    }

    return 0;
}

/* ==================== logfile_close() ==================== */ 
void logfile_close(logfile_t *logfile)
{
    if ( logfile->file_handle > 0 ){
        close(logfile->file_handle);
        logfile->file_handle = -1;
    }
}

/* ==================== logfile_write() ==================== */ 
int logfile_write(logfile_t *logfile, const char *buf, uint32_t buf_size)
{
    assert(buf != NULL && buf_size > 0);
    int writed = -1;
    if ( logfile->file_handle > 0 ){
        writed = write(logfile->file_handle, buf, buf_size);
        if ( writed < buf_size ){
            error_log("Write logfile(%d): %s failed. errno:%d ", logfile->id, logfile->logfile_name, errno);
        } else {
            __sync_add_and_fetch(&logfile->file_size, writed);
        }
    }

    return writed;
}

/* ==================== logfile_read() ==================== */ 
int logfile_read(logfile_t *logfile, const char *buf, uint32_t buf_size)
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

