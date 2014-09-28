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
#include "message.h"
#include "daemon.h"
#include "client.h"

static char program_name[] = "legolas";
static int msec0, msec1;

/*UNUSED static sem_t sem;*/

typedef struct{
    int id;

    int is_execute;

    const char *ip;
    int port;
    int op_code;
    const char *key;
    const char *filename;
    int start_index;
    int total_files;

    int is_daemon;
    int log_level;
    int threads;
} program_options_t;
    
typedef struct thread_args_t {
    int id;
    client_t *client;
} thread_args_t;

int client_execute(client_t *client);

int client_run_task(client_t *client, int id);
int test_run_task(client_t *client, int id);

/*#define run_task client_run_task*/
#define run_task test_run_task

/* ==================== client_thread() ==================== */ 
static void* client_thread(void *arg)
{
    thread_args_t *t_args = (thread_args_t*)arg;
    UNUSED int ret;
    /*ret = start_connect(t_args->client, &callbacks, t_args->id);*/
    ret = run_task(t_args->client, t_args->id);

    return NULL;
}

/* ==================== start_client_threads() ==================== */ 
int start_client_threads(client_t *client)
{
    int ret = 0;
    int threads = client->nthreads;

    /*sem_init(&sem, 0, threads);*/

    /* -------- Create threads -------- */
    pthread_t tid[threads];
    thread_args_t t_args[threads];
    int i;
    for ( i = 0 ; i < threads ; i++ ){
        /*pthread_attr_t attr;*/
        /*pthread_attr_init(&attr);*/
        /*phread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);*/

        t_args[i].id = i;
        t_args[i].client = client;
        ret = pthread_create(&tid[i], NULL, client_thread, &t_args[i]);
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
/*int start_client_normal(program_options_t *program_options)*/
int start_client_normal(client_t *client)
{
    int ret;

    /*ret = start_connect(client, &callbacks, 0);*/
    ret = run_task(client, 0);

    return ret;
}

/* ==================== runclient() ==================== */ 
int runclient(program_options_t *program_options)
{
    UNUSED int is_execute = program_options->is_execute;
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

    /* ---------- New client ---------- */
    client_t *client = client_new(program_options->ip, 
            program_options->port, 
            program_options->op_code,  
            program_options->key, 
            program_options->filename, 
            program_options->start_index,
            program_options->total_files,
            program_options->threads);

    if ( client->op_code == MSG_OP_WRITE ){
        FILE *file = fopen(client->filename, "rb");
        if ( file == NULL ){
            error_log("fopen() failed. file:%s", client->filename);
            return -1;
        }

        fseek(file, 0, SEEK_END);
        uint32_t file_size = ftell(file);
        fseek(file, 0, SEEK_SET);

        char *buf = zmalloc(file_size);
        memset(buf, 0, sizeof(file_size));
        uint32_t readed = fread(buf, 1, file_size, file); 
        if ( readed != file_size ){
            error_log("fread() failed. readed:%d file_size:%d", readed, file_size);
            fclose(file);
            return -1;
        }

        fclose(file);

        client->file_data = buf;
        client->file_size = file_size;
    }

    /* -------- start_client (normal or thread)-------- */

    int ret;

    if ( is_execute == 1 ) {
        ret = client_execute(client);
    } else {
        if ( program_options->threads > 0 ){
            ret = start_client_threads(client);
        } else {
            ret = start_client_normal(client);
        }
    }


    client_free(client);

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
                -e, --execute           execute single command\n\
                -s, --server            specify the remote server\n\
                -p, --port              specify the listen port number\n\
                -w, --write             upload file to server\n\
                -r, --read              download file from server\n\
                -x, --delete            delete file in server\n\
                -k, --key               key of the file\n\
                -b, --start             start of count\n\
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
	{"execute", no_argument, NULL, 'e'},
	{"server", required_argument, NULL, 's'},
	{"port", required_argument, NULL, 'p'},
	{"write", no_argument, NULL, 'w'},
	{"read", no_argument, NULL, 'r'},
	{"delete", no_argument, NULL, 'x'},
	{"key", required_argument, NULL, 'k'},
	{"start", required_argument, NULL, 'b'},
	{"count", required_argument, NULL, 'n'},
	{"threads", required_argument, NULL, 'u'},
	{"daemon", no_argument, NULL, 'd'},
	{"verbose", no_argument, NULL, 'v'},
	{"trace", no_argument, NULL, 't'},
	{"help", no_argument, NULL, 'h'},

	{NULL, 0, NULL, 0},
};
static const char *short_options = "es:p:wrxk:b:n:u:dvth";

/* ==================== main() ==================== */ 
int main(int argc, char *argv[])
{

    program_options_t program_options;
    memset(&program_options, 0, sizeof(program_options_t));

    program_options.is_execute = 0;
    program_options.ip = "127.0.0.1";
	program_options.port = DEFAULT_PORT;
    program_options.filename = "data/32K.dat";
    program_options.key = "default";
    program_options.threads = 0;
    program_options.start_index = 0;
    program_options.is_daemon = 0;
    program_options.log_level = LOG_INFO;
    program_options.op_code = MSG_OP_NONE;

	int ch, longindex;
	while ((ch = getopt_long(argc, argv, short_options, long_options,
				 &longindex)) >= 0) {
		switch (ch) {
            case 'e':
                program_options.is_execute = 1;
                break;
            case 's':
                program_options.ip = optarg;
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
            case 'x':
                program_options.op_code = MSG_OP_DEL;
                break;
            case 'k':
                program_options.key = optarg;
                break;
            case 'b':
                program_options.start_index = atoi(optarg);
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
		program_options.filename = argv[optind];

    return runclient(&program_options);

    
    /*printf("Before fork.\n");*/
    /*return daemon_fork(daemon_loop, (void*)&program_options); */
    /*return daemon_fork4(daemon_loop, daemon_loop, daemon_loop, daemon_loop, (void*)&program_options); */
}

