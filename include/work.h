#ifndef __WORK_H__
#define __WORK_H__

#include <stdint.h>
#include "list.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct work_queue_t work_queue_t;

typedef void (*work_func_t)(work_queue_t *);
/*typedef void (*work_func_t)(void *);*/

int work_queue_get_id(work_queue_t *wq);

void work_queue_set_id(work_queue_t *wq, int id);
work_queue_t *init_work_queue(work_func_t fn, int interval);

//void enqueue_work(work_queue_t *wq, struct list_head *w_list);
void enqueue_work(work_queue_t *wq, void *entry);
void *dequeue_work(work_queue_t *wq);
uint32_t get_work_queue_count(work_queue_t *wq);
void exit_work_queue(work_queue_t *wq);

void remove_work(work_queue_t *wq, struct list_head *w_list);

#ifdef __cplusplus
}
#endif

#endif
