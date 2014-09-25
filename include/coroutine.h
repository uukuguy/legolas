#ifndef __COROUTINE__
#define __COROUTINE__

#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>

#include "list.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct coroutine_t coroutine_t;

typedef void* coroutine_entry_func_t(void *opaque);

coroutine_t *coroutine_create(coroutine_entry_func_t *entry);
void coroutine_delete(coroutine_t *co_);
void _coroutine_enter(coroutine_t *coroutine, void *opaque);
void _coroutine_yield(void);
coroutine_t *coroutine_self(void);
int in_coroutine(void);
void* coroutine_self_data(void);

#ifdef __cplusplus
}
#endif

#endif
