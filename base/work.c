/*
 * Copyright (C) 2011 MORITA Kazutaka <morita.kazutaka@gmail.com>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */
#include <unistd.h>
#include <pthread.h>
/*#include "pcl.h"*/
#include "list.h"
#include "util.h"
#include "work.h"
#include "zmalloc.h"
#include "logger.h"
#include "adlist.h"

static LIST_HEAD(work_queue_list);

struct work_queue {
	struct list_head worker_queue_siblings;

	pthread_cond_t pending_cond;
	pthread_mutex_t pending_lock;
	struct list_head q;
    list *queue;

	work_func_t fn;
	int interval;

	int stop;

	pthread_t worker_thread;
};

void remove_work(struct work_queue *wq, struct list_head *w_list){
    pthread_mutex_lock(&wq->pending_lock);
    list_del(w_list);
    pthread_mutex_unlock(&wq->pending_lock);
}

void *dequeue_work(struct work_queue *wq)
{
    void *nodeData = NULL;

    pthread_mutex_lock(&wq->pending_lock);

    if ( listLength(wq->queue) > 0 ){
        listNode *first = listFirst(wq->queue);
        nodeData = listNodeValue(first);
        listDelNode(wq->queue, first);
    }

    pthread_mutex_unlock(&wq->pending_lock);

    return nodeData;
}

void enqueue_work(struct work_queue *wq, void *entry)
{

    pthread_mutex_lock(&wq->pending_lock);
    listAddNodeTail(wq->queue, entry);
    pthread_mutex_unlock(&wq->pending_lock);

	pthread_cond_signal(&wq->pending_cond);
}

uint32_t get_work_queue_count(struct work_queue *wq)
{
    return wq->queue->len;
}

/*void enqueue_work(struct work_queue *wq, struct list_head *w_list)*/
/*{*/

    /*pthread_mutex_lock(&wq->pending_lock);*/

    /*list_add_tail(w_list, &wq->q);*/

    /*pthread_mutex_unlock(&wq->pending_lock);*/

	/*pthread_cond_signal(&wq->pending_cond);*/
/*}*/

static void *worker_routine(void *arg)
{
	struct work_queue *wq = (struct work_queue*)arg;

    /*if ( co_thread_init() != 0 ){*/
        /*error_log("Call co_thread_init() failed.");*/
        /*pthread_exit(NULL);*/
    /*}*/

    while (!wq->stop) {
        if (wq->interval)
            usleep(wq->interval * 1000);

        pthread_mutex_lock(&wq->pending_lock);
retest:
        if (wq->stop) {
            pthread_mutex_unlock(&wq->pending_lock);
            pthread_exit(NULL);
        }

        if ( listLength(wq->queue) == 0 ) {
            pthread_cond_wait(&wq->pending_cond, &wq->pending_lock);
            goto retest;
        }

        pthread_mutex_unlock(&wq->pending_lock);

        if ( wq->fn ){
            wq->fn(wq);

            /*void *nodeData = NULL;*/
            /*while ( (nodeData = dequeue_work(wq)) != NULL ){*/
                /*wq->fn(nodeData);*/
            /*}*/
        }
    }

    /*co_thread_cleanup();*/

	pthread_exit(NULL);
}

/*static void *worker_routine(void *arg)*/
/*{*/
	/*struct work_queue *wq = (struct work_queue*)arg;*/
	/*struct list_head list;*/

    /*if ( co_thread_init() != 0 ){*/
        /*error_log("Call co_thread_init() failed.");*/
        /*pthread_exit(NULL);*/
    /*}*/

	/*while (!wq->stop) {*/
		/*if (wq->interval)*/
			/*usleep(wq->interval * 1000);*/

		/*pthread_mutex_lock(&wq->pending_lock);*/
/*retest:*/
		/*if (wq->stop) {*/
			/*pthread_mutex_unlock(&wq->pending_lock);*/
			/*pthread_exit(NULL);*/
		/*}*/

        /*if (list_empty(&wq->q)) {*/
			/*pthread_cond_wait(&wq->pending_cond, &wq->pending_lock);*/
			/*goto retest;*/
		/*}*/

		/*INIT_LIST_HEAD(&list);*/
		/*list_splice_init(&wq->q, &list);*/

		/*pthread_mutex_unlock(&wq->pending_lock);*/

		/*if (!list_empty(&list))*/
			/*wq->fn(&list);*/
	/*}*/

    /*co_thread_cleanup();*/

	/*pthread_exit(NULL);*/
/*}*/

struct work_queue *init_work_queue(work_func_t fn, int interval)
{
	int ret;
	struct work_queue *wq;

	wq = (struct work_queue*)zmalloc(sizeof(*wq));
	if (!wq)
		return NULL;

	wq->fn = fn;
	wq->interval = interval;
	INIT_LIST_HEAD(&wq->q);

    wq->queue = listCreate();

	pthread_cond_init(&wq->pending_cond, NULL);

	pthread_mutex_init(&wq->pending_lock, NULL);

	ret = pthread_create(&wq->worker_thread, NULL, worker_routine, wq);
	if (ret) {
		fprintf(stderr, "failed to create a worker thread, %s\n",
			strerror(ret));
		goto destroy_threads;
	}

	list_add(&wq->worker_queue_siblings, &work_queue_list);

	return wq;
destroy_threads:
	wq->stop = 1;

	pthread_join(wq->worker_thread, NULL);

	/* destroy_cond_mutex: */
	pthread_cond_destroy(&wq->pending_cond);
	pthread_mutex_destroy(&wq->pending_lock);

	return NULL;
}

void exit_work_queue(struct work_queue *wq)
{
	pthread_mutex_lock(&wq->pending_lock);
	wq->stop = 1;
	pthread_mutex_unlock(&wq->pending_lock);
	pthread_cond_broadcast(&wq->pending_cond);

	pthread_join(wq->worker_thread, NULL);

	pthread_cond_destroy(&wq->pending_cond);
	pthread_mutex_destroy(&wq->pending_lock);

    listRelease(wq->queue);

	wq->stop = 0;
}
