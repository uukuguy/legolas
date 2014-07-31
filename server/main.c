/**
 * @file   main.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-05-05 14:25:18
 * 
 * @brief  
 * 
 * 
 */

#include "logger.h"
#include "daemon.h"
#include "legolas.h"
#include "server.h"
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <getopt.h>

static char program_name[] = "legolasd";

static void usage(int status)
{
    if ( status )
        fprintf(stderr, "Try `%s --help' for more information.\n",
                program_name);
    else {
        printf("Usage: %s [OPTION] [PATH]\n", program_name);
        printf("Legolas Daemon\n\
                -p, --port              specify the listen port number\n\
                -d, --daemon            run in the daemon mode\n\
                -v, --verbose           print debug messages\n\
                -t, --trace             print trace messages\n\
                -h, --help              display this help and exit\n\
");
    }
    exit(status);
}

static struct option const long_options[] = {
	/* common options */
	{"port", required_argument, NULL, 'p'},
	{"daemon", no_argument, NULL, 'd'},
	{"verbose", no_argument, NULL, 'v'},
	{"trace", no_argument, NULL, 't'},
	{"help", no_argument, NULL, 'h'},

	{NULL, 0, NULL, 0},
};
static const char *short_options = "p:dvth";

extern int start_listen(int port, const char *data_dir); /* in server.c */

typedef struct program_options_t {
    int port;
    const char *data_dir;
    const char *log_dir;
    int is_daemon;
    int log_level;

} program_options_t;

/* ==================== runserver() ==================== */ 
int runserver(program_options_t *program_options)
{
    int port = program_options->port;
    const char *data_dir = program_options->data_dir;
    const char *log_dir = program_options->log_dir;
    int is_daemon = program_options->is_daemon;
    int log_level = program_options->log_level;

    char logfile[PATH_MAX];
    strncpy(logfile, log_dir, sizeof(logfile));
    strncat(logfile, "/legolasd.log", sizeof(logfile) - strlen(logfile) - 1);
    if (log_init(program_name, LOG_SPACE_SIZE, is_daemon, log_level, logfile))
        return -1;

    notice_log("==> Start Legolas Server.");

    pth_init();

    int ret = start_listen(port, data_dir);

    pth_kill();

    notice_log("~~> End Legolas Server.");

    return ret;
}

/* ==================== daemon_loop() ==================== */ 
int daemon_loop(void *data)
{
    return runserver((program_options_t *)data);
}

/* ==================== main() ==================== */ 
int main(int argc, char* argv[])
{

    program_options_t program_options;

	program_options.port = DEFAULT_PORT;
	program_options.data_dir = "./data";
    /*program_options.log_dir = "/Users/jwsu/sandbox/geek/udbfs/log";*/
    program_options.log_dir = "/tmp";
    program_options.is_daemon = 0;
    program_options.log_level = LOG_INFO;

	int ch, longindex;
	while ((ch = getopt_long(argc, argv, short_options, long_options,
				 &longindex)) >= 0) {
		switch (ch) {
		case 'p':
			program_options.port = atoi(optarg);
			break;
        case 'd':
            program_options.is_daemon = 1;
            break;
        case 'v':
            program_options.log_level = LOG_DEBUG;
            break;
		case 't':
            program_options.log_level = LOG_TRACE;
			break;
		case 'h':
			usage(0);
			break;
		default:
			usage(1);
			break;
		}
	}

	if (optind != argc)
		program_options.data_dir = argv[optind];

    if ( program_options.is_daemon )
        return daemon_fork(daemon_loop, (void*)&program_options); 
    else
        return runserver(&program_options);
}

