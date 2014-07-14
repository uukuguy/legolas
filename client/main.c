/**
 * @file   main.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-05-15 15:27:57
 * 
 * @brief  
 * 
 * 
 */

#include "common.h"
#include "legolas.h"
#include "protocol.h"
#include "daemon.h"

static char program_name[] = "legolas";
static int msec0, msec1;

/*UNUSED static sem_t sem;*/

typedef struct{
    const char *server;
    int port;
    const char *key;
    const char *file;
    int op_code;
    int is_daemon;
    int log_level;
    int threads;
    int id;
    int total_files;
} program_options_t;

int start_connect(int clientid, const char *server, int port, int op_code, const char *key, const char *file, int total_files); 
/* ==================== client_thread() ==================== */ 
static void* client_thread(void *arg)
{
    program_options_t *program_options = (program_options_t*)arg;

    int id = program_options->id;
    const char *server = program_options->server;
    int port = program_options->port;
    int op_code = program_options->op_code;
    const char *key = program_options->key;
    const char *file = program_options->file;
    int total_files = program_options->total_files;

    /*notice_log("enter client_thread(). id:%d", id);*/

    UNUSED int ret;
    ret = start_connect(id, server, port, op_code, key, file, total_files);

    /*notice_log("Ready to exit client_thread(). clientid:%d", id);*/

    return NULL;
}

/* ==================== start_client_threads() ==================== */ 
int start_client_threads(program_options_t *program_options)
{
    int ret = 0;
    int threads = program_options->threads;

    /*sem_init(&sem, 0, threads);*/

    /* -------- Create threads -------- */
    pthread_t tid[threads];
    program_options_t po[threads];
    int i;
    for ( i = 0 ; i < threads ; i++ ){
        /*pthread_attr_t attr;*/
        /*pthread_attr_init(&attr);*/
        /*phread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);*/

        memcpy(&po[i], program_options, sizeof(program_options_t));
        po[i].id = i;
        ret = pthread_create(&tid[i], NULL, client_thread, &po[i]);
        /*pthread_attr_destroy(&attr);*/
        if ( ret != 0 ){
            error_log("pthread_create() thread:%d failed. errno:%d", i, errno);
        } else {
            trace_log("phread_create() thread:%d OK.", i);
        }
    }

    /* -------- Join Threads -------- */
    for ( i = 0 ; i < threads ; i++ ){
        void *pret = NULL;
        ret = pthread_join(tid[i], &pret);
        if ( ret != 0 ){
            error_log("pthread_join() failed. i:%d", i);
        } else {
            trace_log("pthread_join() OK. i:%d", i);
        }
    }

    /*do{*/
        /*ret = sem_wait(&sem);*/
    /*} while (ret == -1 && errno == EINTR );*/

    /*sem_destroy(&sem);*/

    return ret;
}

/* ==================== start_client_normal() ==================== */ 
int start_client_normal(program_options_t *program_options)
{
    const char *server = program_options->server;
    int port = program_options->port;
    const char *file = program_options->file;
    const char *key = program_options->key;
    int op_code = program_options->op_code;
    int total_files = program_options->total_files;

    int ret;
    ret = start_connect(0, server, port, op_code, key, file, total_files);
    return ret;
}

/* ==================== runclient() ==================== */ 
int runclient(program_options_t *program_options)
{
    int is_daemon = program_options->is_daemon;
    int log_level = program_options->log_level;

    /* -------- Init logger -------- */
    /*char logfile[PATH_MAX];*/
    /*strncpy(logfile, log_dir, sizeof(logfile));*/
    /*strncat(logfile, "/legolasd.log", sizeof(logfile) - strlen(logfile) - 1);*/
    const char *logfile = "/tmp/legolas.log";
    if (log_init(program_name, LOG_SPACE_SIZE, is_daemon, log_level, logfile))
        return -1;

    notice_log("==> Start Legolas Client. op_code:%d", program_options->op_code);

    /* -------- Begin Timing -------- */
    struct timeval tv; 
    gettimeofday(&tv, NULL); 
    msec0 = tv.tv_sec * 1000 + tv.tv_usec / 1000; 

    /* -------- start_client (normal or thread)-------- */

    int ret;
    if ( program_options->threads > 0 ){
        ret = start_client_threads(program_options);
    } else {
        ret = start_client_normal(program_options);
    }

    /* -------- End Timing -------- */
    gettimeofday(&tv, NULL); 
    msec1 = tv.tv_sec * 1000 + tv.tv_usec / 1000; 

    notice_log("========> Total Time: %d msec.<========", msec1 - msec0);
    notice_log("~~> End Legolas Client.");

    return ret;
}

/* ==================== daemon_loop() ==================== */ 
int daemon_loop(void *data)
{
    printf("In daemon");
    notice_log("In daemon_loop()");
    int i;
    for ( i = 0 ; i < 3000 ; i++ ){
        runclient((program_options_t *)data);
    }
    return 0;
}

/* ==================== usage() ==================== */ 
static void usage(int status)
{
    if ( status )
        fprintf(stderr, "Try `%s --help' for more information.\n",
                program_name);
    else {
        printf("Usage: %s [OPTION] [PATH]\n", program_name);
        printf("Legolas Client\n\
                -s, --server            specify the remote server\n\
                -p, --port              specify the listen port number\n\
                -w, --write             upload file to server\n\
                -r, --read              download file from server\n\
                -e, --delete            delete file in server\n\
                -k, --key               key of the file\n\
                -n, --count             count of loop\n\
                -u, --threads           count of threads\n\
                -d, --daemon            run in the daemon mode. \n\
                -v, --verbose           print debug messages\n\
                -t, --trace             print trace messages\n\
                -h, --help              display this help and exit\n\
");
    }
    exit(status);
}

static struct option const long_options[] = {
	/* common options */
	{"server", required_argument, NULL, 's'},
	{"port", required_argument, NULL, 'p'},
	{"write", no_argument, NULL, 'w'},
	{"read", no_argument, NULL, 'r'},
	{"delete", no_argument, NULL, 'e'},
	{"key", required_argument, NULL, 'k'},
	{"count", required_argument, NULL, 'n'},
	{"threads", required_argument, NULL, 'u'},
	{"daemon", no_argument, NULL, 'd'},
	{"verbose", no_argument, NULL, 'v'},
	{"trace", no_argument, NULL, 't'},
	{"help", no_argument, NULL, 'h'},

	{NULL, 0, NULL, 0},
};
static const char *short_options = "s:p:wrek:n:u:dvth";

/* ==================== main() ==================== */ 
int main(int argc, char *argv[])
{
    program_options_t program_options;

    program_options.server = "127.0.0.1";
	program_options.port = DEFAULT_PORT;
    program_options.file = "data/32K.dat";
    program_options.key = "default";
    program_options.threads = 0;
    program_options.is_daemon = 0;
    program_options.log_level = LOG_INFO;
    program_options.op_code = MSG_OP_NONE;

	int ch, longindex;
	while ((ch = getopt_long(argc, argv, short_options, long_options,
				 &longindex)) >= 0) {
		switch (ch) {
		case 's':
			program_options.server = optarg;
			break;
		case 'p':
			program_options.port = atoi(optarg);
			break;
        case 'w':
            program_options.op_code = MSG_OP_WRITE;
            break;
        case 'r':
            program_options.op_code = MSG_OP_READ;
            break;
        case 'e':
            program_options.op_code = MSG_OP_DEL;
            break;
		case 'k':
			program_options.key = optarg;
			break;
		case 'n':
			program_options.total_files = atoi(optarg);
            if ( program_options.total_files < 0 ) {
                program_options.total_files = 1;
            }
			break;
		case 'u':
			program_options.threads = atoi(optarg);
            if ( program_options.threads < 0 ) {
                program_options.threads = 1;
            }
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
		program_options.file = argv[optind];

    return runclient(&program_options);

    
    /*printf("Before fork.\n");*/
    /*return daemon_fork(daemon_loop, (void*)&program_options); */
    /*return daemon_fork4(daemon_loop, daemon_loop, daemon_loop, daemon_loop, (void*)&program_options); */
}

