#ifndef __COROUTINE__
#define __COROUTINE__

#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>

#include "list.h"

#ifdef __cplusplus
extern "C" {
#endif

struct coroutine;

typedef void* coroutine_entry_func_t(void *opaque);

struct coroutine *coroutine_create(coroutine_entry_func_t *entry);
void coroutine_delete(struct coroutine *co_);
void coroutine_enter(struct coroutine *coroutine, void *opaque);
void coroutine_yield(void);
struct coroutine *coroutine_self(void);
int in_coroutine(void);
void* coroutine_self_data(void);

#ifdef __cplusplus
}
#endif

#endif
