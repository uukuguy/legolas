/**
 * @file   main.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-07 23:47:06
 * 
 * @brief  
 * 
 * 
 */

#include "common.h"
#include "daemon.h"
#include "filesystem.h"
#include "sysinfo.h"
#include "logger.h"

static char program_name[] = "edbroker";

typedef struct{
    const char *frontend;
    const char *backend;

    int is_daemon;
    int log_level;
} program_options_t;

static struct option const long_options[] = {
	{"frontend", required_argument, NULL, 'f'},
	{"backend", required_argument, NULL, 'b'},
	{"threads", required_argument, NULL, 'u'},
	{"daemon", no_argument, NULL, 'd'},
	{"verbose", no_argument, NULL, 'v'},
	{"trace", no_argument, NULL, 't'},
	{"help", no_argument, NULL, 'h'},

	{NULL, 0, NULL, 0},
};
static const char *short_options = "f:b:u:dvth";

extern int run_broker(const char *frontend, const char *backend, int verbose);

/* ==================== daemon_loop() ==================== */ 
int daemon_loop(void *data)
{
    notice_log("In daemon_loop()");

    const program_options_t *po = (const program_options_t *)data;
    return run_broker(po->frontend, po->backend, po->log_level >= LOG_DEBUG ? 1 : 0);
}

/* ==================== usage() ==================== */ 
static void usage(int status)
{
    if ( status )
        fprintf(stderr, "Try `%s --help' for more information.\n",
                program_name);
    else {
        printf("Usage: %s [OPTION] [PATH]\n", program_name);
        printf("Everdata Worker\n\
                -f, --frontend          specify the edbroker frontend endpoint\n\
                -b, --backend          specify the edbroker backend endpoint\n\
                -u, --threads           count of threads\n\
                -d, --daemon            run in the daemon mode. \n\
                -v, --verbose           print debug messages\n\
                -t, --trace             print trace messages\n\
                -h, --help              display this help and exit\n\
");
    }
    exit(status);
}

int main(int argc, char *argv[])
{
    program_options_t po;
    memset(&po, 0, sizeof(program_options_t));

    /*po.frontend = "tcp://127.0.0.1:19977";*/
    /*po.backend = "tcp://127.0.0.1:19978";*/
    po.frontend = "tcp://*:19977";
    po.backend = "tcp://*:19978";

    po.is_daemon = 0;
    po.log_level = LOG_INFO;

	int ch, longindex;
	while ((ch = getopt_long(argc, argv, short_options, long_options,
				 &longindex)) >= 0) {
		switch (ch) {
            case 'f':
                po.frontend = optarg;
                break;
            case 'b':
                po.backend = optarg;
                break;
            case 'd':
                po.is_daemon = 1;
                break;
            case 'v':
                po.log_level = LOG_DEBUG;
                break;
            case 't':
                po.log_level = LOG_TRACE;
                break;
            case 'h':
                usage(0);
                break;
            default:
                usage(1);
                break;
        }
	}

    /* -------- Init logger -------- */
    char root_dir[NAME_MAX];
    get_instance_parent_full_path(root_dir, NAME_MAX);

    char log_dir[NAME_MAX];
    sprintf(log_dir, "%s/log", root_dir);
    mkdir_if_not_exist(log_dir);

    char logfile[PATH_MAX];
    sprintf(logfile, "%s/%s.log", log_dir, program_name);

    if (log_init(program_name, LOG_SPACE_SIZE, po.is_daemon, po.log_level, logfile))
        return -1;

    if ( po.is_daemon ){
        return daemon_fork(daemon_loop, (void*)&po); 
    } else 
        return run_broker(po.frontend, po.backend, po.log_level >= LOG_DEBUG ? 1 : 0);
}

