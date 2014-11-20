/**
 * @file  cboost.h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-18 14:34:21
 * 
 * @brief  
 * 
 * 
 */

#ifndef __CBOOST_H__
#define __CBOOST_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "common.h"

/* ================ iterator ================ */
typedef struct g_iterator_t g_iterator_t;

extern g_iterator_t *g_iterator_next(g_iterator_t *iter);
extern g_iterator_t *g_iterator_prev(g_iterator_t *iter);
extern void *g_iterator_get(g_iterator_t *iter);
extern void g_iterator_set(g_iterator_t *iter, void *element);
extern void g_iterator_erase(g_iterator_t *iter);
extern void g_iterator_free(g_iterator_t *iter);

/* ================ vector ================ */
typedef struct g_vector_t  g_vector_t;

extern g_vector_t *g_vector_new(void);
extern void g_vector_free(g_vector_t *vec);
extern void g_vector_clear(g_vector_t *vec);
extern size_t g_vector_size(g_vector_t *vec);
extern void g_vector_push_back(g_vector_t *vec, void *element);
extern void *g_vector_pop_front(g_vector_t *vec);
extern size_t g_vector_erase(g_vector_t *vec, size_t element_idx);
extern void *g_vector_get_element(g_vector_t *vec, size_t element_idx);
extern void *g_vector_get_first(g_vector_t *vec);
extern void *g_vector_get_last(g_vector_t *vec);
extern g_iterator_t *g_vector_begin(g_vector_t *vector);
extern g_iterator_t *g_vector_end(g_vector_t *vector);


/* ================ list ================ */
typedef struct g_list_t g_list_t;

extern g_list_t *g_list_new(void);
extern void g_list_free(g_list_t *list);
extern void g_list_clear(g_list_t *list);
extern size_t g_list_size(g_list_t *list);
extern void g_list_push_back(g_list_t *list, void *element);
extern void g_list_push_front(g_list_t *list, void *element);
extern void g_list_pop_back(g_list_t *list);
extern void g_list_pop_front(g_list_t *list);
extern g_iterator_t *g_list_begin(g_list_t *list);
extern g_iterator_t *g_list_end(g_list_t *list);
extern g_iterator_t *g_list_erase(g_list_t *list, g_iterator_t *iter);
extern int g_list_empty(g_list_t *list);


/* ================ queue ================ */


/* ================ stack ================ */


/* ================ map ================ */

#ifdef __cplusplus
}
#endif

#endif /* __CBOOST_H__ */

