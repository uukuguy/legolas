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
 *
 * This code is based on coroutine-ucontext.c and qemu-coroutine.c from QEMU:
 *   Copyright (C) 2006 Anthony Liguori <anthony@codemonkey.ws>
 *   Copyright (C) 2011 Stefan Hajnoczi <stefanha@linux.vnet.ibm.com>
 *   Copyright (C) 2011 Kevin Wolf <kwolf@redhat.com>
 */

#include <stdlib.h>
#include <setjmp.h>
#include <stdint.h>
#include <pthread.h>
#include <ucontext.h>
#include <errno.h>
#include <assert.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/socket.h>

#include "util.h"
#include "coroutine.h"
#include "zmalloc.h"
#include "logger.h"

enum co_action {
	COROUTINE_YIELD = 1,
	COROUTINE_TERMINATE = 2,
};

/* Maximum free pool size prevents holding too many freed coroutines */
#ifdef COROUTINE_DEBUG
#define POOL_MAX_SIZE   1
#else
#define POOL_MAX_SIZE   1024
#endif

#define STACK_MAX_SIZE (1 << 16)  /* 64 KB */

typedef struct coroutine_t {
	coroutine_entry_func_t *entry;
	void *entry_arg;
	coroutine_t *caller;
	struct list_head pool_next;
	struct list_head co_queue_next;
} coroutine_t;

typedef struct co_ucontext_t {
	coroutine_t base;
	void *stack;
	jmp_buf env;
} co_ucontext_t;

/**
 * Per-thread coroutine bookkeeping
 */
typedef struct co_thread_state_t{
	/** Currently executing coroutine */
	coroutine_t *current;

	/** Free list to speed up creation */
	struct list_head pool;
	unsigned int pool_size;

	/** The default coroutine */
	co_ucontext_t leader;
} co_thread_state_t;

static pthread_key_t thread_state_key;

static enum co_action coroutine_switch(coroutine_t *from,
				       coroutine_t *to,
				       enum co_action action);

/*
 * va_args to makecontext() must be type 'int', so passing
 * the pointer we need may require several int args. This
 * union is a quick hack to let us do that
 */
union cc_arg {
	void *p;
	int i[2];
};

static co_thread_state_t *coroutine_get_thread_state(void)
{
	co_thread_state_t *s = (co_thread_state_t*)pthread_getspecific(thread_state_key);

	if (!s) {
		s = (co_thread_state_t*)zmalloc(sizeof(*s));
		if (!s)
			abort();
		s->current = &s->leader.base;
		INIT_LIST_HEAD(&s->pool);
		pthread_setspecific(thread_state_key, s);
	}
	return s;
}

static void coroutine_thread_cleanup(void *opaque)
{
    /*co_thread_state_t *s = (co_thread_state_t*)opaque;*/
    /*coroutine_t *co;*/
    /*coroutine_t *tmp;*/

    /*list_for_each_entry_safe(co, tmp, &s->pool, pool_next) {*/
        /*[>co_ucontext_t *ucontext = container_of(co, co_ucontext_t, base);<]*/
        /*[>void *stack = ucontext->stack;<]*/

        /*void *stack = container_of(co, co_ucontext_t, base)->stack;*/
        /*free(stack);*/
        /*free(co);*/
    /*}*/

    /*free(s);*/
}

static void __attribute__((constructor)) coroutine_init(void)
{
	int ret;

	ret = pthread_key_create(&thread_state_key, coroutine_thread_cleanup);
	if (ret != 0) {
		error_log("unable to create leader key: %m\n");
		abort();
	}
}

static void coroutine_trampoline(int i0, int i1)
{
	union cc_arg arg;
	co_ucontext_t *self;
	coroutine_t *co;

	arg.i[0] = i0;
	arg.i[1] = i1;
	self = arg.p;
	co = &self->base;

	/* Initialize longjmp environment and switch back the caller */
	if (!setjmp(self->env))
		longjmp(*(jmp_buf *)co->entry_arg, 1);

	for (;;) {
		co->entry(co->entry_arg);
		coroutine_switch(co, co->caller, COROUTINE_TERMINATE);
	}
}

#ifdef COROUTINE_DEBUG

#define MAGIC_NUMBER 0x1234567890123456

static void init_stack(co_ucontext_t *co)
{
	uint64_t *stack = co->stack;
	int i;

	for (i = 0; i < STACK_MAX_SIZE / sizeof(stack[0]); i++)
		stack[i] = MAGIC_NUMBER;
}

static int get_stack_size(co_ucontext_t *co)
{
	uint64_t *stack = co->stack;
	int i;

	for (i = 0; i < STACK_MAX_SIZE / sizeof(stack[0]); i++)
		if (stack[i] != MAGIC_NUMBER)
			break;

	if (i == 0) {
		error_log("stack overflow\n");
		abort();
	}

	return STACK_MAX_SIZE - i * sizeof(stack[0]);
}

#endif

static coroutine_t *__coroutine_new(void)
{
	const size_t stack_size = STACK_MAX_SIZE;
	co_ucontext_t *co;
	ucontext_t old_uc, uc;
	jmp_buf old_env;
	union cc_arg arg = {0};

	/* The ucontext functions preserve signal masks which incurs a
	 * system call overhead.  setjmp()/longjmp() does not preserve
	 * signal masks but only works on the current stack.  Since we
	 * need a way to create and switch to a new stack, use the
	 * ucontext functions for that but setjmp()/longjmp() for
	 * everything else.
	 */

	if (getcontext(&uc) == -1)
		abort();

	co = zmalloc(sizeof(*co));
	if (!co)
		abort();
	co->stack = zmalloc(stack_size);
	if (!co->stack)
		abort();
#ifdef COROUTINE_DEBUG
	init_stack(co);
#endif
	co->base.entry_arg = &old_env; /* stash away our jmp_buf */

	uc.uc_link = &old_uc;
	uc.uc_stack.ss_sp = co->stack;
	uc.uc_stack.ss_size = stack_size;
	uc.uc_stack.ss_flags = 0;

	arg.p = co;

	makecontext(&uc, (void (*)(void))coroutine_trampoline,
		    2, arg.i[0], arg.i[1]);

	/* swapcontext() in, longjmp() back out */
	if (!setjmp(old_env))
		swapcontext(&old_uc, &uc);

	return &co->base;
}

static coroutine_t *coroutine_new(void)
{
	co_thread_state_t *s = coroutine_get_thread_state();
	coroutine_t *co;

	if (!list_empty(&s->pool)) {
		co = list_first_entry(&s->pool, coroutine_t, pool_next);
		list_del(&co->pool_next);
		s->pool_size--;
	} else
		co = __coroutine_new();

	return co;
}

void coroutine_delete(coroutine_t *co_)
{
	co_thread_state_t *s = coroutine_get_thread_state();
	co_ucontext_t *co = container_of(co_, co_ucontext_t, base);

#ifdef COROUTINE_DEBUG
	error_log("%d bytes are consumed\n", get_stack_size(co));
#endif

    if (s->pool_size < POOL_MAX_SIZE) {
        list_add(&co->base.pool_next, &s->pool);
        co->base.caller = NULL;
        s->pool_size++;
        return;
    }

	zfree(co->stack);
	zfree(co);
}

static enum co_action coroutine_switch(coroutine_t *from_,
				       coroutine_t *to_,
				       enum co_action action)
{
	co_ucontext_t *from = container_of(from_, co_ucontext_t, base);
	co_ucontext_t *to = container_of(to_, co_ucontext_t, base);
	co_thread_state_t *s = coroutine_get_thread_state();
	int ret;

	s->current = to_;

	ret = setjmp(from->env);
	if (ret == 0)
		longjmp(to->env, action);

	return ret;
}

void* coroutine_self_data(void)
{
    return coroutine_self()->entry_arg;
}

coroutine_t *coroutine_self(void)
{
	co_thread_state_t *s = coroutine_get_thread_state();

	return s->current;
}

int in_coroutine(void)
{
	co_thread_state_t *s = pthread_getspecific(thread_state_key);

	return s && s->current->caller;
}


coroutine_t *coroutine_create(coroutine_entry_func_t *entry)
{
	coroutine_t *co = coroutine_new();
	co->entry = entry;
	return co;
}

static void coroutine_swap(coroutine_t *from, coroutine_t *to)
{
	enum co_action ret;

	ret = coroutine_switch(from, to, COROUTINE_YIELD);

	switch (ret) {
	case COROUTINE_YIELD:
		return;
	case COROUTINE_TERMINATE:
		coroutine_delete(to);
		return;
	default:
		abort();
	}
}

void coroutine_enter(coroutine_t *co, void *opaque)
{
	coroutine_t *self = coroutine_self();

	if (unlikely(co->caller)) {
		error_log("Co-routine re-entered recursively\n");
		abort();
	}

	co->caller = self;
	co->entry_arg = opaque;
	coroutine_swap(self, co);
}

void coroutine_yield(void)
{
	coroutine_t *self = coroutine_self();
	coroutine_t *to = self->caller;

	if (unlikely(!to)) {
		error_log("Co-routine is yielding to no one\n");
		abort();
	}

	self->caller = NULL;
	coroutine_swap(self, to);
}
