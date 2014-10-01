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
#include "filesystem.h"
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <getopt.h>
#include <string.h>

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
                -s, --storage           set storage type.(kvdb, logfile, none)\n\
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
    {"storage", required_argument, NULL, 's'},
	{"help", no_argument, NULL, 'h'},

	{NULL, 0, NULL, 0},
};
static const char *short_options = "p:dvth";

typedef struct program_options_t {
    int listen_port;
    const char *data_dir;
    enum eVnodeStorageType storage_type;

    /*const char *log_dir;*/
    /*int is_daemon;*/
    /*int log_level;*/

} program_options_t;

/* ==================== run_server() ==================== */ 
int run_server(program_options_t *program_options)
{
    notice_log("==> Start Legolas Server.");

    pth_init();

    int ret = -1;

    server_options_t server_options;
    server_options.listen_port = program_options->listen_port;
    server_options.data_dir = program_options->data_dir;
    server_options.storage_type = program_options->storage_type;

    server_t *server = server_new(&server_options);
    if ( server_init(server) == 0 ){
        ret = server_listen(server);
    }
    server_free(server);

    pth_kill();

    notice_log("~~> End Legolas Server.");

    return ret;
}

/* ==================== daemon_loop() ==================== */ 
int daemon_loop(void *data)
{
    return run_server((program_options_t *)data);
}

/* ==================== init_logger() ==================== */ 
int init_logger(int is_daemon, int log_level)
{
    char root_dir[NAME_MAX];
    get_instance_parent_full_path(root_dir, NAME_MAX);

    char log_dir[NAME_MAX];
    sprintf(log_dir, "%s/log", root_dir);
    mkdir_if_not_exist(log_dir);

    char logfile[PATH_MAX];
    sprintf(logfile, "%s/legolasd.log", log_dir);

    int ret = log_init(program_name, LOG_SPACE_SIZE, is_daemon, log_level, logfile);

    return ret;
}

/* ==================== main() ==================== */ 
int main(int argc, char* argv[])
{

    program_options_t program_options;

	program_options.listen_port = DEFAULT_PORT;
	program_options.data_dir = "./data";

    int is_daemon = 0;
    int log_level = LOG_INFO;
    const char *sz_storage_type = NULL;

	int ch, longindex;
	while ((ch = getopt_long(argc, argv, short_options, long_options,
				 &longindex)) >= 0) {
		switch (ch) {
		case 'p':
			program_options.listen_port = atoi(optarg);
			break;
        case 'd':
            is_daemon = 1;
            break;
        case 'v':
            log_level = LOG_DEBUG;
            break;
		case 't':
            log_level = LOG_TRACE;
			break;
        case 's':
            sz_storage_type = optarg;
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

    /* -------- storage_type -------- */
    eVnodeStorageType storage_type = STORAGE_KVDB;
    if ( sz_storage_type != NULL ){
        if ( strcmp(sz_storage_type, "logfile") == 0 ){
            storage_type = STORAGE_LOGFILE;
        } else if ( strcmp(sz_storage_type, "kvdb" ) == 0 ){
            storage_type = STORAGE_KVDB;
        } else if ( strcmp(sz_storage_type, "none" ) == 0 ){
            storage_type = STORAGE_NONE;
        }
    }
    program_options.storage_type = storage_type;

    /* -------- logger -------- */
    if ( init_logger(is_daemon, log_level) != 0 ){
        printf("init_logger() failed.\n");
        return -1;
    }

    /* -------- run_server -------- */
    if ( is_daemon )
        return daemon_fork(daemon_loop, (void*)&program_options); 
    else
        return run_server(&program_options);
}

