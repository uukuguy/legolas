/**
 * @file   lockfree_queue.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-05-30 20:04:45
 * 
 * @brief  
 * 
 * 
 */
#ifndef __LOCKFREE_QUEUE_H__
#define __LOCKFREE_QUEUE_H__

struct lockfree_queue_t;

struct lockfree_queue_t *lockfree_queue_create(void);
void lockfree_queue_delete(struct lockfree_queue_t *queue);

void lockfree_queue_enqueue(struct lockfree_queue_t *queue, void *entry);
void *lockfree_queue_dequeue(struct lockfree_queue_t *queue);
int lockfree_queue_is_empty(struct lockfree_queue_t *queue);
int lockfree_queue_total_elements(struct lockfree_queue_t *queue);

#endif /* __LOCKFREE_QUEUE_H__ */

