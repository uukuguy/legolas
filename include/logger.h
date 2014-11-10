/*
 * Copyright (C) 2009-2011 Nippon Telegraph and Telephone Corporation.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License version
 * 2 as published by the Free Software Foundation.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * This code is based on log.h from Linux target framework (tgt).
 *   Copyright (C) 2004 Dmitry Yusupov, Alex Aizman
 */
#ifndef LOGGER_H
#define LOGGER_H

#include <sys/sem.h>
#include <sys/syslog.h>
#include <time.h>
#include <sys/time.h>

#include "util.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef LOG_TRACE
#define LOG_TRACE LOG_DEBUG + 1
#endif

union logger_semun {
	int val;
	struct semid_ds *buf;
	unsigned short int *array;
	struct seminfo *__buf;
};

#define LOG_SPACE_SIZE (8 * 1024 * 1024)
#define MAX_MSG_SIZE 512

struct logmsg {
	short int prio;
	void *next;
	char *str;
};

struct logarea {
	int empty;
	int active;
	void *head;
	void *tail;
	void *start;
	void *end;
	char *buff;
	int semid;
	union logger_semun semarg;
	int fd;
};

extern int log_init(const char *progname, int size, int daemon, int level,
		    const char *outfile);
extern void log_close (void);
extern void dump_logmsg (void *);
extern void log_write(int prio, const char *fmt, ...)
	__attribute__ ((format (printf, 2, 3)));
extern void log_flush(void);

extern int is_debug;
extern int is_trace;
extern int is_daemon;

#define _log_message(priority, priority_str, color, fmt, args...) \
do {									\
    struct timeval tv; \
    gettimeofday(&tv, NULL); \
    int msec = tv.tv_usec / 1000; \
    struct tm tm; \
    localtime_r(&tv.tv_sec, &tm); \
    char szNow[32]; \
    sprintf(szNow, "%04d-%02d-%02d %02d:%02d:%02d.%03d", \
            tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday, \
            tm.tm_hour, tm.tm_min, tm.tm_sec, msec); \
	log_write(priority, "%s " color "| " priority_str " |\033[0m %s:%d:%s " color "| MSG | " fmt " \033[0m\n", szNow, __FILE__, __LINE__, __func__, ##args);	\
    if (likely(is_daemon)) \
    log_flush(); \
} while (0)


	/*log_write(priority, color "%s | " priority_str " | %s:%d:%s | " fmt "\n\033[0m", szNow, __FILE__, __LINE__, __func__, ##args);	 */
	/*log_write(priority, color "%s | " priority_str " | " fmt " (%s:%d:%s)\n\033[0m", szNow, ##args, __FILE__, __LINE__, __func__);	*/

#define error_log(fmt, args...) \
    _log_message(LOG_ERR, "ERROR ", "\033[1;31m", fmt, ##args);

#define warning_log(fmt, args...) \
    _log_message(LOG_WARNING, "WARN  ", "\033[1;35m", fmt, ##args);

#define notice_log(fmt, args...) \
    _log_message(LOG_NOTICE, "NOTICE", "\033[1;36m", fmt, ##args);

#define info_log( fmt, args...) \
    _log_message(LOG_INFO, "INFO  ", "\033[1;32m", fmt, ##args);

#define debug_log(fmt, args...) \
	if (likely(is_debug))						\
    _log_message(LOG_DEBUG, "DEBUG ", "\033[1;37m", fmt, ##args);

#define trace_log(fmt, args...) \
	if (likely(is_trace))						\
    _log_message(LOG_TRACE, "TRACE ", "\033[1;34m", fmt, ##args);

static inline void TRACE_DATA_TO_FILE(const char *filename, void *data, size_t len){
    if ( likely(is_trace) ){ 
        FILE *_f0 = fopen(filename, "wb+"); 
        fwrite(data, 1, len, _f0); 
        fclose(_f0); 
        trace_log("TRACE_DATA_TO_FILE(). filename:%s len:%zu", filename, (size_t)len); 
    }
}

#ifdef __cplusplus
}
#endif


#endif	/* LOG_H */
