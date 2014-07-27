/**
 * @file   legolas.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-07-07 01:49:51
 * 
 * @brief  
 * 
 * 
 */

#ifndef __LEGOLAS_H__
#define __LEGOLAS_H__

#include "zmalloc.h"
#include "list.h"
#include "adlist.h"
#include "uv.h"
#include "work.h"
#include "logger.h"
#include "message.h"
//#include "../server/vnode.h"
#include <time.h>

#define DEFAULT_PORT 16076



/* Fully close a loop */

static void close_walk_cb(uv_handle_t* handle, void* arg) {
  if (!uv_is_closing(handle))
    uv_close(handle, NULL);
}

UNUSED static void close_loop(uv_loop_t* loop) {
  uv_walk(loop, close_walk_cb, NULL);
  uv_run(loop, UV_RUN_DEFAULT);
}

/* This macro cleans up the main loop. This is used to avoid valgrind
 * warnings about memory being "leaked" by the main event loop.
 */
#define MAKE_VALGRIND_HAPPY(loop)           \
  do {                                  \
    close_loop(loop);      \
    uv_loop_delete(loop);  \
  } while (0)

#endif /* __LEGOLAS_H__ */


