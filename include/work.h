#ifndef __WORK_H__
#define __WORK_H__

#include "list.h"

#ifdef __cplusplus
extern "C" {
#endif

struct work;
struct work_queue;

//typedef void (*work_func_t)(struct list_head *);
typedef void (*work_func_t)(void *);

struct work_queue *init_work_queue(work_func_t fn, int interval);
//void enqueue_work(struct work_queue *wq, struct list_head *w_list);
void enqueue_work(struct work_queue *wq, void *entry);
void exit_work_queue(struct work_queue *wq);
void remove_work(struct work_queue *wq, struct list_head *w_list);

#ifdef __cplusplus
}
#endif

#endif
