/**
 * @file   main.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-08 01:14:11
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

static char program_name[] = "edclient";

typedef struct{
    const char *endpoint;
    int op_code;
    int total_files;
    int total_threads;

    const char *key;
    const char *filename;
    int start_index;

    int log_level;
} program_options_t;

static struct option const long_options[] = {
	{"endpoint", required_argument, NULL, 'e'},
	{"write", no_argument, NULL, 'w'},
	{"read", no_argument, NULL, 'r'},
	{"delete", no_argument, NULL, 'x'},
	{"key", required_argument, NULL, 'k'},
	{"start", required_argument, NULL, 's'},
	{"count", required_argument, NULL, 'n'},
	{"threads", required_argument, NULL, 'u'},
	{"verbose", no_argument, NULL, 'v'},
	{"trace", no_argument, NULL, 't'},
	{"help", no_argument, NULL, 'h'},

	{NULL, 0, NULL, 0},
};
static const char *short_options = "e:wrxk:s:n:u:vth";

extern int run_client(const char *endpoint, int op_code, int total_threads, uint32_t total_files, const char *key, const char *filename, int verbose);

/* ==================== daemon_loop() ==================== */ 
int daemon_loop(void *data)
{
    const program_options_t *po = (const program_options_t *)data;
    return run_client(po->endpoint, po->op_code, po->total_threads, po->total_files, po->key, po->filename, po->log_level >= LOG_DEBUG ? 1 : 0);
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
                -e, --endpoint          specify the edbroker endpoint\n\
                -u, --threads           count of threads\n\
                -w, --write             upload file to server\n\
                -r, --read              download file from server\n\
                -x, --delete            delete file in server\n\
                -k, --key               key of the file\n\
                -s, --start             start of count\n\
                -n, --count             count of loop\n\
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

    po.endpoint = "tcp://127.0.0.1:19977";
    po.op_code = 1;
    po.total_threads = 100;
    po.total_files = 1000;
    po.start_index = 0;
    po.key = "default";
    po.filename = "./data/samples/32K.dat";

    po.log_level = LOG_INFO;

	int ch, longindex;
	while ((ch = getopt_long(argc, argv, short_options, long_options,
				 &longindex)) >= 0) {
		switch (ch) {
            case 'e':
                po.endpoint = optarg;
                break;
            case 'u':
                po.total_threads = atoi(optarg);
                if ( po.total_threads < 0 ) {
                    po.total_threads = 1;
                }
                break;
            case 'w':
                po.op_code = 1;
                break;
            case 'r':
                po.op_code = 2;
                break;
            case 'x':
                po.op_code = 3;
                break;
            case 'k':
                po.key = optarg;
                break;
            case 's':
                po.start_index = atoi(optarg);
                break;
            case 'n':
                po.total_files = atoi(optarg);
                if ( po.total_files < 0 ) {
                    po.total_files = 1;
                }
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

    if (log_init(program_name, LOG_SPACE_SIZE, 0, po.log_level, logfile))
        return -1;

    int msec0, msec1;

    /* -------- Begin Timing -------- */
    struct timeval tv; 
    gettimeofday(&tv, NULL); 
    msec0 = tv.tv_sec * 1000 + tv.tv_usec / 1000; 

    int rc = run_client(po.endpoint, po.op_code, po.total_threads, po.total_files, po.key, po.filename, po.log_level >= LOG_DEBUG ? 1 : 0);

    /* -------- End Timing -------- */
    gettimeofday(&tv, NULL); 
    msec1 = tv.tv_sec * 1000 + tv.tv_usec / 1000; 

    notice_log("========> Total Time: %d.%03d sec.<========", (msec1 - msec0) / 1000, (msec1 - msec0) % 1000);
    notice_log("~~> End EverData Client.");

    return rc;
}

