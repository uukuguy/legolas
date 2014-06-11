/**
 * @file   lockfree_queue.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-05-30 20:06:51
 * 
 * @brief  
 * 
 * 
 */

#include "lockfree_queue.h"
#include "liblfds611.h"
#include "zmalloc.h"

typedef struct lockfree_queue_t {
    struct lfds611_queue_state *sq;

} lockfree_queue_t;


struct lockfree_queue_t *lockfree_queue_create(void)
{

    struct lockfree_queue_t *queue = zmalloc(sizeof(struct lockfree_queue_t));
    if ( lfds611_queue_new(&queue->sq, 10) ){
        return queue;
    } else {
        zfree(queue);
        return NULL;
    }
}

void lockfree_queue_delete(struct lockfree_queue_t *queue)
{
    lfds611_queue_delete(queue->sq, NULL, NULL);
    zfree(queue);
}


void lockfree_queue_enqueue(struct lockfree_queue_t *queue, void *element)
{
    lfds611_queue_enqueue(queue->sq, element);
}

void *lockfree_queue_dequeue(struct lockfree_queue_t *queue)
{
    void *element = NULL;
    if ( lfds611_queue_dequeue(queue->sq, &element) )
        return element;
    else
        return NULL;
}

int lockfree_queue_is_empty(struct lockfree_queue_t *queue)
{
    return (lockfree_queue_total_elements(queue) == 0);
}

int lockfree_queue_total_elements(struct lockfree_queue_t *queue)
{
    int element_count = 0;
    lfds611_queue_query(queue->sq, LFDS611_QUEUE_QUERY_ELEMENT_COUNT, NULL, &element_count);
    return element_count;
}

