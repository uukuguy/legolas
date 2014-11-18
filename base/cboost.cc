/**
 * @file  cboost.cc
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-18 14:33:05
 * 
 * @brief  
 * 
 * 
 */

#include "cboost.h"
#include <string>
#include "zmalloc.h"
#include <vector>
#include <list>
#include <map>
#include <set>


/* ================ iterator ================ */
typedef struct cb_iterator_t{
} cb_iterator_t;

void cb_iterator_first(cb_iterator_t *iter)
{
}

void cb_iterator_next(cb_iterator_t *iter)  
{
}

void cb_iterator_prev(cb_iterator_t *iter)
{
}

void cb_iterator_last(cb_iterator_t *iter)
{
}

int cb_iterator_is_end(cb_iterator_t *iter)
{
    return 1;
}

void cb_iterator_free(cb_iterator_t *iter)
{
}


/* ================ vector ================ */
typedef struct cb_vector_t{
    typedef std::vector<void*> Vector;
    Vector vec;
} cb_vector_t;

cb_vector_t *cb_vector_new(void)
{
    cb_vector_t *vec = (cb_vector_t*)zmalloc(sizeof(cb_vector_t));
    memset(vec, 0, sizeof(cb_vector_t));

    //vec->vec = std::vector<void*>();

    return vec;
}

void cb_vector_free(cb_vector_t *vec)
{
    if ( vec != NULL ){
        zfree(vec);
    }
}

void cb_vector_clear(cb_vector_t *vec)
{
    vec->vec.clear();
}

size_t cb_vector_size(cb_vector_t *vec)
{
    return vec->vec.size();
}

void cb_vector_push_back(cb_vector_t *vec, void *item)
{
    vec->vec.push_back(item);
}

size_t cb_vector_erase(cb_vector_t *vec, size_t item_idx)
{
    cb_vector_t::Vector::iterator it = vec->vec.erase(vec->vec.begin() + item_idx);
    return it - vec->vec.begin();
}

void *cb_vector_get_item(cb_vector_t *vec, size_t item_idx)
{
    if ( item_idx >= 0 && item_idx < cb_vector_size(vec) ){
        return vec->vec[item_idx];
    } else {
        return NULL;
    }
}

void *cb_vector_get_first(cb_vector_t *vec)
{
    cb_vector_t::Vector::iterator it = vec->vec.begin();
    if ( it != vec->vec.end() )
        return *it;
    else
        return NULL;
}

void *cb_vector_get_last(cb_vector_t *vec)
{
    cb_vector_t::Vector::reverse_iterator it = vec->vec.rbegin();
    if ( it != vec->vec.rend() )
        return *it;
    else
        return NULL;
}

void *cb_vector_pop_front(cb_vector_t *vec)
{
    void *item = NULL;
    if ( vec->vec.size() > 0 ){
        item = *(vec->vec.begin());
        if ( item != NULL ){
            cb_vector_erase(vec, 0);
        }
    }

    return item;
}

cb_iterator_t *cb_vector_new_iterator(cb_vector_t *vec)
{
    return NULL;
}

