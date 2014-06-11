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
 * This code is based on log.c from Linux target framework (tgt):
 *   Copyright (C) 2002-2003 Ardis Technolgies <roman@ardistech.com>
 */
#include <ctype.h>
#include <fcntl.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <syslog.h>
#include <signal.h>
#include <errno.h>
#include <time.h>
#include <sys/shm.h>
#include <sys/ipc.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#ifdef USE_PRCTL
#include <sys/prctl.h>
#endif

#include "logger.h"

#define LOGDBG 0

#if LOGDBG
#define logdbg(file, fmt, args...) fprintf(file, fmt, ##args)
#else
#define logdbg(file, fmt, args...) do {} while (0)
#endif

static int log_enqueue(int prio, const char *fmt, va_list ap)
	__attribute__ ((format (printf, 2, 0)));
static void dolog(int prio, const char *fmt, va_list ap)
	__attribute__ ((format (printf, 2, 0)));

static struct logarea *la;
static const char *log_name;
int is_debug = 0;
int is_trace = 0;
int is_daemon = 1;
static pid_t pid;
static key_t semkey;

static int logarea_init (int size)
{
	int shmid;

	logdbg(stderr,"enter logarea_init\n");

	if ((shmid = shmget(IPC_PRIVATE, sizeof(struct logarea),
			    0644 | IPC_CREAT | IPC_EXCL)) == -1) {
		syslog(LOG_ERR, "shmget logarea failed %d", errno);
		return 1;
	}

	la = shmat(shmid, NULL, 0);
	if (!la) {
		syslog(LOG_ERR, "shmat logarea failed %d", errno);
		return 1;
	}

	shmctl(shmid, IPC_RMID, NULL);

	if (size < MAX_MSG_SIZE)
		size = LOG_SPACE_SIZE;

	if ((shmid = shmget(IPC_PRIVATE, size,
			    0644 | IPC_CREAT | IPC_EXCL)) == -1) {
		syslog(LOG_ERR, "shmget msg failed %d", errno);
		shmdt(la);
		return 1;
	}

	la->start = shmat(shmid, NULL, 0);
	if (!la->start) {
		syslog(LOG_ERR, "shmat msg failed %d", errno);
		shmdt(la);
		return 1;
	}
	memset(la->start, 0, size);

	shmctl(shmid, IPC_RMID, NULL);

	la->empty = 1;
	la->end = (char *)la->start + size;
	la->head = la->start;
	la->tail = la->start;

	if ((shmid = shmget(IPC_PRIVATE, MAX_MSG_SIZE + sizeof(struct logmsg),
			    0644 | IPC_CREAT | IPC_EXCL)) == -1) {
		syslog(LOG_ERR, "shmget logmsg failed %d", errno);
		shmdt(la->start);
		shmdt(la);
		return 1;
	}
	la->buff = shmat(shmid, NULL, 0);
	if (!la->buff) {
		syslog(LOG_ERR, "shmat logmsgfailed %d", errno);
		shmdt(la->start);
		shmdt(la);
		return 1;
	}

	shmctl(shmid, IPC_RMID, NULL);

	if ((la->semid = semget(semkey, 1, 0666 | IPC_CREAT)) < 0) {
		syslog(LOG_ERR, "semget failed %d", errno);
		shmdt(la->buff);
		shmdt(la->start);
		shmdt(la);
		return 1;
	}

	la->semarg.val=1;
	if (semctl(la->semid, 0, SETVAL, la->semarg) < 0) {
		syslog(LOG_ERR, "semctl failed %d", errno);
		shmdt(la->buff);
		shmdt(la->start);
		shmdt(la);
		return 1;
	}

	return 0;
}

static void free_logarea (void)
{
	if (la->fd >= 0)
		close(la->fd);
	semctl(la->semid, 0, IPC_RMID, la->semarg);
	shmdt(la->buff);
	shmdt(la->start);
	shmdt(la);
}

#if LOGDBG
static void dump_logarea (void)
{
	struct logmsg * msg;

	logdbg(stderr, "\n==== area: start addr = %p, end addr = %p ====\n",
		la->start, la->end);
	logdbg(stderr, "|addr     |next     |prio|msg\n");

	for (msg = (struct logmsg *)la->head; (void *)msg != la->tail;
	     msg = msg->next)
		logdbg(stderr, "|%p |%p |%i   |%s\n", (void *)msg, msg->next,
				msg->prio, (char *)&msg->str);

	logdbg(stderr, "|%p |%p |%i   |%s\n", (void *)msg, msg->next,
			msg->prio, (char *)&msg->str);

	logdbg(stderr, "\n\n");
}
#endif

static int log_enqueue(int prio, const char *fmt, va_list ap)
{
	int len, fwd;
	char *p, buff[MAX_MSG_SIZE];
	struct logmsg *msg;
	struct logmsg *lastmsg;

	lastmsg = (struct logmsg *)la->tail;

	if (!la->empty) {
		fwd = sizeof(struct logmsg) +
		      strlen((char *)&lastmsg->str) * sizeof(char) + 1;
		la->tail = (char *)la->tail + fwd;
	}

	p = buff;

	/*if (la->fd != -1) {*/
		/*time_t t;*/
		/*struct tm *tmp;*/

		/*t = time(NULL);*/
		/*tmp = localtime(&t);*/

		/*strftime(p, MAX_MSG_SIZE, "%b %2d %I:%M:%S ", tmp);*/
		/*p += strlen(p);*/
	/*}*/

	vsnprintf(p, MAX_MSG_SIZE - strlen(p), fmt, ap);
	len = strlen(buff) * sizeof(char) + 1;

	/* not enough space on tail : rewind */
	if (la->head <= la->tail &&
	    (len + sizeof(struct logmsg)) > ((char *)la->end - (char *)la->tail)) {
		logdbg(stderr, "enqueue: rewind tail to %p\n", la->tail);
			la->tail = la->start;
	}

	/* not enough space on head : drop msg */
	if (la->head > la->tail &&
	    (len + sizeof(struct logmsg)) > ((char *)la->head - (char *)la->tail)) {
		logdbg(stderr, "enqueue: log area overrun, drop msg\n");

		if (!la->empty)
			la->tail = lastmsg;

		return 1;
	}

	/* ok, we can stage the msg in the area */
	la->empty = 0;
	msg = (struct logmsg *)la->tail;
	msg->prio = prio;
	memcpy((void *)&msg->str, buff, len);
	lastmsg->next = la->tail;
	msg->next = la->head;

	logdbg(stderr, "enqueue: %p, %p, %i, %s\n", (void *)msg, msg->next,
		msg->prio, (char *)&msg->str);

#if LOGDBG
	dump_logarea();
#endif
	return 0;
}

static int log_dequeue(void *buff)
{
	struct logmsg * src = (struct logmsg *)la->head;
	struct logmsg * dst = (struct logmsg *)buff;
	struct logmsg * lst = (struct logmsg *)la->tail;
	int len;

	if (la->empty)
		return 1;

	len = strlen((char *)&src->str) * sizeof(char) +
		sizeof(struct logmsg) + 1;

	dst->prio = src->prio;
	memcpy(dst, src,  len);

	if (la->tail == la->head)
		la->empty = 1; /* we purge the last logmsg */
	else {
		la->head = src->next;
		lst->next = la->head;
	}
	logdbg(stderr, "dequeue: %p, %p, %i, %s\n",
		(void *)src, src->next, src->prio, (char *)&src->str);

	memset((void *)src, 0,  len);

	return la->empty;
}

/*
 * this one can block under memory pressure
 */
static void log_syslog (void * buff)
{
	struct logmsg * msg = (struct logmsg *)buff;

	if (la->fd >= 0)
		write(la->fd, (char *)&msg->str, strlen((char *)&msg->str));
	else
		syslog(msg->prio, "%s", (char *)&msg->str);
}

static void dolog(int prio, const char *fmt, va_list ap)
{
	struct sembuf ops;

	if (la) {
		ops.sem_num = 0;
		ops.sem_flg = SEM_UNDO;
		ops.sem_op = -1;
		if (semop(la->semid, &ops, 1) < 0) {
			syslog(LOG_ERR, "semop up failed %m");
			return;
		}

		log_enqueue(prio, fmt, ap);

		ops.sem_op = 1;
		if (semop(la->semid, &ops, 1) < 0) {
			syslog(LOG_ERR, "semop down failed");
			return;
		}
	} else {
		char p[MAX_MSG_SIZE];
		int len;
		len = vsnprintf(p, sizeof(p), fmt, ap);
		write(STDERR_FILENO, p, len);
		fflush(stderr);
	}
}

void log_write(int prio, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	dolog(prio, fmt, ap);
	va_end(ap);
}

void log_flush(void)
{
	struct sembuf ops;

	while (!la->empty) {
		ops.sem_num = 0;
		ops.sem_flg = SEM_UNDO;
		ops.sem_op = -1;
		if (semop(la->semid, &ops, 1) < 0) {
			syslog(LOG_ERR, "semop up failed");
			exit(1);
		}

		log_dequeue(la->buff);

		ops.sem_op = 1;
		if (semop(la->semid, &ops, 1) < 0) {
			syslog(LOG_ERR, "semop down failed");
			exit(1);
		}
		log_syslog(la->buff);
	}
}

static void log_sigsegv(void)
{
	error_log("logger exits abnormally, pid:%d\n", getpid());
	log_flush();
	closelog();
	free_logarea();
	exit(1);
}

int log_init(const char *program_name, int size, int daemon, int level,
	     const char *outfile)
{

    if ( level == LOG_TRACE ){
        is_debug = 1;
        is_trace = 1;
    }
    if ( level == LOG_DEBUG ){
        is_debug = 1;
        is_trace = 0;
    }
    is_daemon = daemon;

	logdbg(stderr,"enter log_init\n");
	log_name = program_name;

	semkey = random();

	if (is_daemon) {
		struct sigaction sa_old;
		struct sigaction sa_new;
		int fd;

		if (outfile) {
			fd = open(outfile, O_CREAT | O_RDWR | O_APPEND, 0644);
			if (fd < 0)
				syslog(LOG_ERR, "failed to open %s\n", outfile);
		} else {
			fd = -1;
			openlog(log_name, 0, LOG_DAEMON);
			setlogmask (LOG_UPTO (LOG_DEBUG));
		}

		if (logarea_init(size)) {
			syslog(LOG_ERR, "failed to initialize the logger\n");
			return 1;
		}

		la->active = 1;
		la->fd = fd;
		pid = fork();
		if (pid < 0) {
			syslog(LOG_ERR, "fail to fork the logger\n");
			return 1;
		} else if (pid) {
			syslog(LOG_WARNING,
			       "Target daemon logger with pid=%d started!\n", pid);
			return 0;
		}

		fd = open("/dev/null", O_RDWR);
		if (fd < 0) {
			syslog(LOG_ERR, "failed to open /dev/null: %s\n",
			       strerror(errno));
			exit(1);
		}

		dup2(fd, 0);
		dup2(fd, 1);
		dup2(fd, 2);
		setsid();
		if (chdir("/") < 0) {
			syslog(LOG_ERR, "failed to chdir to '/': %s\n",
			       strerror(errno));
			exit(1);
		}

		/* flush on daemon's crash */
		sa_new.sa_handler = (void*)log_sigsegv;
		sigemptyset(&sa_new.sa_mask);
		sa_new.sa_flags = 0;
		sigaction(SIGSEGV, &sa_new, &sa_old );

#ifdef USE_PRCTL
        const char *logger_process_name = "legolas_logger";
        prctl(PR_SET_PDEATHSIG, SIGSEGV);
        prctl(PR_SET_NAME, (unsigned long)logger_process_name, NULL, NULL, NULL);
#endif

		while (la->active) {
			log_flush();
			sleep(1);
		}

		exit(0);
	}

	return 0;
}

void log_close(void)
{
	if (la) {
		la->active = 0;
		waitpid(pid, NULL, 0);

		debug_log("logger stopped, pid:%d\n", pid);
		log_flush();
		closelog();
		free_logarea();
	}
}
