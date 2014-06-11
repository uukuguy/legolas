/**
 * @file   session_send.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-05-05 13:25:37
 * 
 * @brief  
 * 
 * 
 */

#include "session.h"
#include "protocol.h"
#include "md5.h"
#include "uv.h"
#include "coroutine.h"
/*#include "pcl.h"*/
/*#include "co_routine.h"*/

#include <pthread.h>
#include <uuid/uuid.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

/* ==================== after_response_to_client() ==================== */ 
UNUSED static void after_response_to_client(uv_write_t *write_rsp, int status) 
{
    SESSION_INFO_FROM_UV_HANDLE(write_rsp, session_info, server_info);

    trace_log("fd(%d) Enter after_response_to_client().", session_fd(session_info));

    /*zfree(write_rsp);*/

    /*pthread_mutex_lock(&session_info->after_response_lock);*/
    /*session_info->is_sending = 0;*/
    /*pthread_mutex_unlock(&session_info->after_response_lock);*/
    /*pthread_cond_signal(&session_info->after_response_cond);*/

    /*session_rx_on(session_info);*/

    trace_log("fd(%d) after_response_to_client() done.", session_fd(session_info));
}

/* ==================== response_to_client() ==================== */ 
UNUSED void response_to_client(struct session_info_t *session_info, enum MSG_RESULT result)
{
    __sync_add_and_fetch(&session_info->finished_works, 1);
    /*uv_async_send(&session_info->async_handle);*/

    return;


    /* -------- response message -------- */
    struct msg_response_t *response = alloc_response(0, result);
    uv_buf_t rbuf = uv_buf_init((char *)response, sizeof(msg_response_t));

    /* -------- write_rsp -------- */
    uv_write_t *write_rsp;
    write_rsp = zmalloc(sizeof(uv_write_t));
    write_rsp->data = session_info;

    /* -------- uv_write -------- */
    /*session_rx_off(session_info);*/
    trace_log("fd(%d) Ready to call uv_write", session_fd(session_info));

    /*uv_read_stop((uv_stream_t*)&session_stream(session_info));*/
    /*pthread_mutex_lock(&session_info->after_response_lock);*/
    /*session_info->is_sending = 1;*/
    int r = uv_write(write_rsp,
            &session_stream(session_info),
            &rbuf,
            1,
            /*NULL);*/
            after_response_to_client);
    /*pthread_mutex_unlock(&session_info->after_response_lock);*/

    if ( r != 0 ) {
        error_log("response failed");
    }

    /*uv_read_start((uv_stream_t*)&session_stream(session_info), session_alloc, after_read);*/

    /*pthread_mutex_lock(&session_info->after_response_lock);*/
    /*while ( session_info->is_sending ){*/
        /*pthread_cond_wait(&session_info->after_response_cond, &session_info->after_response_lock);*/
    /*}*/
    /*pthread_mutex_unlock(&session_info->after_response_lock);*/
}

/* ==================== session_tx_handler() ==================== */ 
void* session_tx_handler(void *opaque)
{
    /*struct session_info_t *session_info = opaque;*/



    /*struct response *rsp;*/
    /*LIST_HEAD(list);*/
/*again:*/
    /*pthread_mutex_lock(&ci->tx_lock);*/
    /*list_splice_init(&ci->rsp_list, &list);*/
    /*pthread_mutex_unlock(&ci->tx_lock);*/

    /*while (!ci->tx_failed && !list_empty(&list))*/
        /*__client_tx_handler(ci, &list);*/

    /*if (ci->tx_failed) {*/
        /*while (!list_empty(&list)) {*/
            /*rsp = list_first_entry(&list, struct response, w_list);*/
            /*list_del(&rsp->w_list);*/

            /*free_response(rsp);*/
        /*}*/
    /*}*/

    /*pthread_mutex_lock(&ci->tx_lock);*/
    /*if (list_empty(&ci->rsp_list))*/
        /*ci->tx_on = 0;*/
    /*pthread_mutex_unlock(&ci->tx_lock);*/

    /*if (ci->tx_on == 0)*/
        /*coroutine_yield();*/
        /*co_resume();*/

    /*goto again;*/
    return NULL;
}

/* ==================== send_response() ==================== */ 
/*void send_response(struct list_head *work_list)*/
void send_response(void *arg)
{
	/*struct client_info *ci, *n;*/

	/*list_for_each_entry_safe(ci, n, work_list, tx_list) {*/
		/*list_del(&ci->tx_list);*/
		/*assert(ci->tx_on == 1);*/

		/*coroutine_enter(ci->tx_co, ci);*/
        /*co_call(ci->tx_co, ci)*/

		/*if (!ci->tx_failed && ci->stop && !too_many_requests(ci)) {*/
			/*ci->stop = 0;*/
			/*client_rx_on(ci);*/
		/*}*/

		/*client_decref(ci);*/
	/*}*/
}

