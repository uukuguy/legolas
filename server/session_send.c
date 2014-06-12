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


/* ==================== response_to_client() ==================== */ 
UNUSED void response_to_client(struct session_info_t *session_info, enum MSG_RESULT result)
{
    __sync_add_and_fetch(&session_info->finished_works, 1);
    /*uv_async_send(&session_info->async_handle);*/
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

/* ==================== send_cob_in_queue() ==================== */ 
void send_cob_in_queue(struct conn_buf_t *cob)
{
    session_info_t *session_info = cob->session_info;

    if ( likely( cob->remain_bytes > 0 ) ) {

        /**
         * coroutine enter session_rx_handler() 
         */
        coroutine_enter(session_info->tx_co, cob);

        /**
         * too many requests or not ?
         */

        if ( too_many_requests(session_info) ) {
            session_info->stop = 1;
        } else {
            session_rx_on(session_info);
        }

    } else {
        /* -------- remain bytes == 0 -------- */
    }

    destroy_cob(cob);

    /*session_finish_saving_buffer(session_info);*/

    /*if ( session_is_waiting(session_info) ){*/
        /*if ( session_info->waiting_for_close == 1 ) { */
            /*pthread_cond_signal(&session_info->recv_pending_cond);*/
        /*}*/
    /*}*/

    pthread_yield();

}

/* ==================== send_response() ==================== */ 
void send_response(struct work_queue *wq)
{
    void *nodeData = NULL;
    while ( (nodeData = dequeue_work(wq)) != NULL ){
       send_cob_in_queue((struct conn_buf_t*)nodeData); 
    }
}

