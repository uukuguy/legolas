#ifndef __WORK_H__
#define __WORK_H__

#include "list.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct work_queue_t work_queue_t;

typedef void (*work_func_t)(work_queue_t *);
/*typedef void (*work_func_t)(void *);*/

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
