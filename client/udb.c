/**
 * @file   udb.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-09-09 13:29:43
 * 
 * @brief  
 * 
 * 
 */

#include "udb.h"
#include "service.h"
#include "session.h"
#include "zmalloc.h"
#include "adlist.h"
#include "adlist.h"
#include "logger.h"

/*#include "react_utils.h"*/

static uint32_t udb_id = 0;

int udb_is_write_done(udb_t *udb)
{
    return udb->total_writed < udb->object_size ? 0 : 1;
}

uint32_t udb_get_writed_bytes(udb_t *udb)
{
    return udb->total_writed;
}

uint32_t udb_get_readed_bytes(udb_t *udb)
{
    return udb->total_readed;
}

int udb_do(udb_t *udb, on_ready_cb on_ready)
{
    assert(udb != NULL);
    if ( on_ready != NULL ){
        udb->on_ready = on_ready;
    }

    int ret = 0;

	pthread_mutex_init(&udb->main_pending_lock, NULL);
	pthread_cond_init(&udb->main_pending_cond, NULL);

    udb_run(udb);

    pthread_mutex_lock(&udb->main_pending_lock);
    pthread_cond_wait(&udb->main_pending_cond, &udb->main_pending_lock);
    pthread_mutex_unlock(&udb->main_pending_lock);

    pthread_mutex_destroy(&udb->main_pending_lock);
    pthread_cond_destroy(&udb->main_pending_cond);

    notice_log("--- udb_do() udb(%d) done! ---", udb->id);

    return ret;
}

void udb_done(udb_t *udb)
{
    assert(udb != NULL);

    /* FIXME */
    /* Check is time to shutdown now? */
    session_t *session = udb->session; 

    /* FIXME 2014-10-15 10:38:05 */
    /*session_rx_off(session);*/

    session->waiting_for_close = 1;
    session_shutdown(session);
}

/* ==================== udb_session_is_idle() ==================== */ 
int udb_session_is_idle(session_t *session)
{
    return 1;
}

/* ==================== udb_session_idle_cb() ==================== */ 
/*void udb_session_idle_cb(uv_idle_t *idle_handle, int status) */
/*{*/
/*}*/

/* ==================== udb_session_timer_cb() ==================== */ 
/*void udb_session_timer_cb(uv_timer_t *timer_handle, int status) */
/*{*/
/*}*/

/* ==================== udb_session_async_cb() ==================== */ 
/*void udb_session_async_cb(uv_async_t *async_handle, int status) */
/*{*/
/*}*/

/* ==================== udb_session_init() ==================== */ 
int udb_session_init(session_t *session)
{
    return 0;
}

/* ==================== udb_session_destroy() ==================== */ 
void udb_session_destroy(session_t *session) 
{
    /* FIXME */
    if ( session != NULL ){
        udb_t *udb = udb(session);
        zfree(session);
        pthread_cond_signal(&udb->main_pending_cond);
    }
}

/* ==================== udb_open_data() ==================== */ 
int udb_open_data(udb_t *udb, const char *key)
{
    return -1;
}

/* ==================== udb_close_data() ==================== */ 
int udb_close_data(udb_t *udb, int handle)
{
    return -1;
}

int udb_handle_write_response(session_t *session, message_t *response);
int udb_handle_read_response(session_t *session, message_t *response);
int udb_handle_delete_response(session_t *session, message_t *response);

int udb_handle_message(session_t *session, message_t *message)
{
    int ret = 0;
    udb_t *udb = udb(session);
 
    trace_log("udb_handle_message(). Has a message. op_code:%d", udb->op_code);
    switch ( udb->op_code ){
        case MSG_OP_WRITE:
            {
                if ( message->msg_type == MSG_TYPE_REQUEST ){
                } else if ( message->msg_type == MSG_TYPE_RESPONSE ) {
                    ret = udb_handle_write_response(session, message);
                }
            } break;
        case MSG_OP_READ:
            {
                if ( message->msg_type == MSG_TYPE_REQUEST ){
                } else if ( message->msg_type == MSG_TYPE_RESPONSE ) {
                    ret = udb_handle_read_response(session, message);
                }
            } break;
        case MSG_OP_DEL:
            {
                if ( message->msg_type == MSG_TYPE_REQUEST ){
                } else if ( message->msg_type == MSG_TYPE_RESPONSE ) {
                    ret = udb_handle_delete_response(session, message);
                }
            } break;
    }


    return ret;
}

/* ==================== udb_on_connect() ==================== */ 
static void udb_on_connect(session_t *session, int status) 
/*static void udb_on_connect(service_t *service, int status) */
{
    UNUSED udb_t *udb= udb(session);
    /*udb_t *udb = (udb_t*)service->parent;*/

    if ( udb->on_ready != NULL ){
        udb->on_ready(udb);
    }

    pthread_cond_signal(&udb->on_ready_cond);
}

/* ==================== udb_loop() ==================== */ 
static int udb_loop(udb_t *udb)
{
    int r = 0;

    /* ---------- New session ---------- */
    session_t *session = session_new(udb->service, NULL);
    udb->session = session;

    r = session_loop(session, udb->ip, udb->port);
    if ( r != 0 ){
        error_log("Call session_loop() failed.");
    }

    udb_free(udb);

    return r;
}

/* ==================== udb_thread_main() ==================== */ 
static void* udb_thread_main(void *arg)
{
    udb_t *udb = (udb_t*)arg;

    trace_log("Enter udb_thread_main().");

    int rc = udb_loop(udb);
    if ( rc != 0 ){
        error_log("udb_creat_session() failed.");
    }

    notice_log("udb_thread_main() %d exit.", udb->id);

    pthread_cond_signal(&udb->main_pending_cond);

    return NULL;
}

/* ==================== udb_run() ==================== */ 
int udb_run(udb_t *udb)
{
    int rc = 0;

    /*udb->react_aggregator = REACT_CREATE_SUBTHREAD_AGGREGATOR();*/
    /*pthread_attr_t attr;*/
    /*pthread_attr_init(&attr);*/
    /*phread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);*/
    rc = pthread_create(&udb->tid, NULL, udb_thread_main, udb);

    pthread_mutex_lock(&udb->on_ready_lock);
    pthread_cond_wait(&udb->on_ready_cond, &udb->on_ready_lock);
    pthread_mutex_unlock(&udb->on_ready_lock);

    /*REACT_DESTROY_SUBTHREAD_AGGREGATOR(udb->react_aggregator);*/

    return rc;
}

/* ==================== udb_exit() ==================== */ 
void udb_exit(udb_t *udb)
{
    pthread_cancel(udb->tid);
}


/* ==================== udb_new() ==================== */ 
udb_t *udb_new(const char *ip, int port, void *user_data)
{

    udb_t *udb = (udb_t*)zmalloc(sizeof(udb_t));
    memset(udb, 0, sizeof(udb_t));

    static session_callbacks_t udb_callbacks = {
        /*.idle_cb = udb_session_idle_cb,*/
        /*.timer_cb = udb_session_timer_cb,*/
        /*.async_cb = udb_session_async_cb,*/
        .is_idle = udb_session_is_idle,
        .handle_message = udb_handle_message,
        .session_init = udb_session_init,
        .session_destroy = udb_session_destroy,

        .on_connect = NULL,

        .consume_sockbuf = NULL,
        .handle_read_response = NULL,
        .session_on_connect_to_server = udb_on_connect,
    };

    const session_callbacks_t *callbacks = &udb_callbacks;

    udb->service = service_new(udb, callbacks);

    udb->id = udb_id;
    __sync_add_and_fetch(&udb_id, 1);

    udb->ip = ip;
    udb->port = port;

    udb->user_data = user_data;
    /*udb->writing_objects = listCreate();*/

	pthread_mutex_init(&udb->on_ready_lock, NULL);
	pthread_cond_init(&udb->on_ready_cond, NULL);

    return udb;
}


void udb_free(udb_t *udb)
{
    if ( udb != NULL ) {

        /*if ( udb->writing_objects != NULL ){*/
            /*listRelease(udb->writing_objects);*/
            /*udb->writing_objects = NULL;*/
        /*}*/

        if ( udb->user_data != NULL ){
            zfree(udb->user_data);
            udb->user_data = NULL;
        }

        service_destroy(udb->service);

        pthread_mutex_destroy(&udb->on_ready_lock);
        pthread_cond_destroy(&udb->on_ready_cond);

        zfree(udb);
    }
}

