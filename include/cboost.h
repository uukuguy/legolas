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
typedef struct cb_iterator_t cb_iterator_t;

extern void cb_iterator_first(cb_iterator_t *iter);  
extern void cb_iterator_next(cb_iterator_t *iter);  
extern void cb_iterator_prev(cb_iterator_t *iter);  
extern void cb_iterator_last(cb_iterator_t *iter);  
extern int cb_iterator_is_end(cb_iterator_t *iter);
extern void cb_iterator_free(cb_iterator_t *iter);

/* ================ vector ================ */
typedef struct cb_vector_t  cb_vector_t;

extern cb_vector_t *cb_vector_new(void);
extern void cb_vector_free(cb_vector_t *vec);
extern void cb_vector_clear(cb_vector_t *vec);
extern size_t cb_vector_size(cb_vector_t *vec);
extern void cb_vector_push_back(cb_vector_t *vec, void *item);
extern size_t cb_vector_erase(cb_vector_t *vec, size_t item_idx);
extern void *cb_vector_get_item(cb_vector_t *vec, size_t item_idx);
extern void *cb_vector_get_first(cb_vector_t *vec);
extern void *cb_vector_get_last(cb_vector_t *vec);
extern void *cb_vector_pop_front(cb_vector_t *vec);


extern cb_iterator_t *cb_vector_new_iterator(cb_vector_t *vec);



/* ================ list ================ */


/* ================ map ================ */

#ifdef __cplusplus
}
#endif

#endif /* __CBOOST_H__ */

