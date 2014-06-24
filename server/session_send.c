/**
 * @file   session_send.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-05-05 13:25:37
 * 
 * @brief  
 * 
 * 
 */

#include "server.h"
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
UNUSED void response_to_client(session_t *session, enum MSG_RESULT result)
{
    __sync_add_and_fetch(&session->finished_works, 1);
    /*uv_async_send(&session->async_handle);*/
}

/* ==================== session_tx_handler() ==================== */ 
void* session_tx_handler(void *opaque)
{
    /*session_t *session = opaque;*/

    /*response *rsp;*/
    /*LIST_HEAD(list);*/
/*again:*/
    /*pthread_mutex_lock(&ci->tx_lock);*/
    /*list_splice_init(&ci->rsp_list, &list);*/
    /*pthread_mutex_unlock(&ci->tx_lock);*/

    /*while (!ci->tx_failed && !list_empty(&list))*/
        /*__client_tx_handler(ci, &list);*/

    /*if (ci->tx_failed) {*/
        /*while (!list_empty(&list)) {*/
            /*rsp = list_first_entry(&list, response, w_list);*/
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
void send_cob_in_queue(conn_buf_t *cob)
{
    session_t *session = cob->session;

    if ( likely( cob->remain_bytes > 0 ) ) {

        /**
         * coroutine enter session_rx_handler() 
         */
        coroutine_enter(session->tx_co, cob);

        /**
         * too many requests or not ?
         */

        if ( too_many_requests(session) ) {
            session->stop = 1;
        } else {
            session_rx_on(session);
        }

    } else {
        /* -------- remain bytes == 0 -------- */
    }

    delete_cob(cob);

    /*session_finish_saving_buffer(session);*/

    /*if ( session_is_waiting(session) ){*/
        /*if ( session->waiting_for_close == 1 ) { */
            /*pthread_cond_signal(&session->recv_pending_cond);*/
        /*}*/
    /*}*/

    pthread_yield();

}

/* ==================== send_response() ==================== */ 
void send_response(work_queue_t *wq)
{
    void *nodeData = NULL;
    while ( (nodeData = dequeue_work(wq)) != NULL ){
       send_cob_in_queue((conn_buf_t*)nodeData); 
    }
}

