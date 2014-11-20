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
#include <map>
#include <set>


/* ================ iterator ================ */
typedef g_iterator_t *(*iterator_next_fn)(g_iterator_t *);
typedef g_iterator_t *(*iterator_prev_fn)(g_iterator_t *);
typedef void *(*iterator_get_fn)(g_iterator_t *);
typedef void (*iterator_set_fn)(g_iterator_t *, void*);
typedef void (*iterator_erase_fn)(g_iterator_t *);

typedef struct g_iterator_t{
    void *container;
    iterator_next_fn next;
    iterator_prev_fn prev;
    iterator_get_fn get;
    iterator_set_fn set;
    iterator_erase_fn erase;
} g_iterator_t;

g_iterator_t *g_iterator_next(g_iterator_t *iter)  
{
    return iter->next(iter);
}

g_iterator_t *g_iterator_prev(g_iterator_t *iter)
{
    return iter->prev(iter);
}

void *g_iterator_get(g_iterator_t *iter)
{
    return iter->get(iter);
}

void g_iterator_set(g_iterator_t *iter, void *element)
{
    iter->set(iter, element);
}

void g_iterator_erase(g_iterator_t *iter)
{
    iter->erase(iter);
}


void g_iterator_free(g_iterator_t *iter)
{
    zfree(iter);
}


/* ================ g_vector_iterator_t ================ */
typedef struct g_vector_iterator_t {
    g_iterator_t baseIterator;
    typedef std::vector<void*>::iterator VectorIterator;
    VectorIterator it;
} g_vector_iterator_t;

g_iterator_t *g_vector_iterator_next(g_iterator_t *iter)
{
    g_vector_iterator_t *vectorIter = (g_vector_iterator_t*)iter;
    ++vectorIter->it;
    return iter;
}

g_iterator_t *g_vector_iterator_prev(g_iterator_t *iter)
{
    g_vector_iterator_t *vectorIter = (g_vector_iterator_t*)iter;
    --vectorIter->it;
    return iter;
}

void *g_vector_iterator_get(g_iterator_t *iter)
{
    g_vector_iterator_t *vectorIter = (g_vector_iterator_t*)iter;
    return *vectorIter->it;
}

void g_vector_iterator_set(g_iterator_t *iter, void *element)
{
    g_vector_iterator_t *vectorIter = (g_vector_iterator_t*)iter;
    *vectorIter->it = element;
}

void g_vector_iterator_erase(g_iterator_t *iter);
/* ---------------- g_vector_iterator_new() ---------------- */
g_vector_iterator_t *g_vector_iterator_new(void* container)
{
    g_vector_iterator_t *listIter = (g_vector_iterator_t*)zmalloc(sizeof(g_vector_iterator_t));
    memset(listIter, 0, sizeof(g_vector_iterator_t));

    listIter->baseIterator.container = container;
    listIter->baseIterator.next = g_vector_iterator_next;
    listIter->baseIterator.prev = g_vector_iterator_prev;
    listIter->baseIterator.get = g_vector_iterator_get;
    listIter->baseIterator.set = g_vector_iterator_set;
    listIter->baseIterator.erase = g_vector_iterator_erase;

    return listIter;
}

/* ---------------- g_vector_iterator_free() ---------------- */
void g_vector_iterator_free(g_vector_iterator_t * vectorIter)
{
    zfree(vectorIter);
}

/* ================ vector ================ */
typedef struct g_vector_t{
    typedef std::vector<void*> Vector;
    Vector vec;
} g_vector_t;

void g_vector_iterator_erase(g_iterator_t *iter)
{
    g_vector_t *vec = (g_vector_t*)iter->container;
    g_vector_iterator_t *vectorIter = (g_vector_iterator_t*)iter;
    vec->vec.erase(vectorIter->it);
}

g_vector_t *g_vector_new(void)
{
    g_vector_t *vec = (g_vector_t*)zmalloc(sizeof(g_vector_t));
    memset(vec, 0, sizeof(g_vector_t));

    //vec->vec = std::vector<void*>();

    return vec;
}

void g_vector_free(g_vector_t *vec)
{
    if ( vec != NULL ){
        zfree(vec);
    }
}

void g_vector_clear(g_vector_t *vec)
{
    vec->vec.clear();
}

size_t g_vector_size(g_vector_t *vec)
{
    return vec->vec.size();
}

void g_vector_push_back(g_vector_t *vec, void *element)
{
    vec->vec.push_back(element);
}

size_t g_vector_erase(g_vector_t *vec, size_t element_idx)
{
    g_vector_t::Vector::iterator it = vec->vec.erase(vec->vec.begin() + element_idx);
    return it - vec->vec.begin();
}

void *g_vector_get_element(g_vector_t *vec, size_t element_idx)
{
    if ( element_idx >= 0 && element_idx < g_vector_size(vec) ){
        return vec->vec[element_idx];
    } else {
        return NULL;
    }
}

void *g_vector_get_first(g_vector_t *vec)
{
    g_vector_t::Vector::iterator it = vec->vec.begin();
    if ( it != vec->vec.end() )
        return *it;
    else
        return NULL;
}

void *g_vector_get_last(g_vector_t *vec)
{
    g_vector_t::Vector::reverse_iterator it = vec->vec.rbegin();
    if ( it != vec->vec.rend() )
        return *it;
    else
        return NULL;
}

void *g_vector_pop_front(g_vector_t *vec)
{
    void *element = NULL;
    if ( vec->vec.size() > 0 ){
        element = *(vec->vec.begin());
        if ( element != NULL ){
            g_vector_erase(vec, 0);
        }
    }

    return element;
}

/* ---------------- g_vector_begin() ---------------- */
g_iterator_t *g_vector_begin(g_vector_t *vector)
{
    g_vector_iterator_t *vectorIter = g_vector_iterator_new(vector);
    vectorIter->it = vector->vec.begin();

    return (g_iterator_t*)vectorIter;
}

/* ---------------- g_vector_end() ---------------- */
g_iterator_t *g_vector_end(g_vector_t *vector)
{
    g_vector_iterator_t *vectorIter = g_vector_iterator_new(vector);
    vectorIter->it = vector->vec.end();

    return (g_iterator_t*)vectorIter;
}

/* ================ list ================ */
#include <list>

/* ================ g_list_iterator_t ================ */
typedef struct g_list_iterator_t {
    g_iterator_t baseIterator;
    typedef std::list<void*>::iterator ListIterator;
    ListIterator it;
} g_list_iterator_t;

g_iterator_t *g_list_iterator_next(g_iterator_t *iter)
{
    g_list_iterator_t *listIter = (g_list_iterator_t*)iter;
    ++listIter->it;
    return iter;
}

g_iterator_t *g_list_iterator_prev(g_iterator_t *iter)
{
    g_list_iterator_t *listIter = (g_list_iterator_t*)iter;
    --listIter->it;
    return iter;
}

void *g_list_iterator_get(g_iterator_t *iter)
{
    g_list_iterator_t *listIter = (g_list_iterator_t*)iter;
    return *listIter->it;
}

void g_list_iterator_set(g_iterator_t *iter, void *element)
{
    g_list_iterator_t *listIter = (g_list_iterator_t*)iter;
    *listIter->it = element;
}

void g_list_iterator_erase(g_iterator_t *iter);
/* ---------------- g_list_iterator_new() ---------------- */
g_list_iterator_t *g_list_iterator_new(void* container)
{
    g_list_iterator_t *listIter = (g_list_iterator_t*)zmalloc(sizeof(g_list_iterator_t));
    memset(listIter, 0, sizeof(g_list_iterator_t));

    listIter->baseIterator.container = container;
    listIter->baseIterator.next = g_list_iterator_next;
    listIter->baseIterator.prev = g_list_iterator_prev;
    listIter->baseIterator.get = g_list_iterator_get;
    listIter->baseIterator.set = g_list_iterator_set;
    listIter->baseIterator.erase = g_list_iterator_erase;

    return listIter;
}

/* ---------------- g_list_iterator_free() ---------------- */
void g_list_iterator_free(g_list_iterator_t * listIter)
{
    zfree(listIter);
}

/* ================ g_list_t ================ */
typedef struct g_list_t {
    typedef std::list<void*> List;
    List list;
} g_list_t;


void g_list_iterator_erase(g_iterator_t *iter)
{
    g_list_t *list = (g_list_t*)iter->container;
    g_list_iterator_t *listIter = (g_list_iterator_t*)iter;
    list->list.erase(listIter->it);
}

/* ---------------- g_list_new() ---------------- */
g_list_t *g_list_new(void)
{
    g_list_t *list = (g_list_t*)zmalloc(sizeof(g_list_t));
    memset(list, 0, sizeof(g_list_t));

    return list;
}

/* ---------------- g_list_free() ---------------- */
void g_list_free(g_list_t *list)
{
    zfree(list);
}

/* ---------------- g_list_clear() ---------------- */
void g_list_clear(g_list_t *list)
{
    list->list.clear();
}

/* ---------------- g_list_size() ---------------- */
size_t g_list_size(g_list_t *list)
{
    return list->list.size();
}

/* ---------------- g_list_push_back() ---------------- */
void g_list_push_back(g_list_t *list, void *element)
{
    list->list.push_back(element);
}

/* ---------------- g_list_push_front() ---------------- */
void g_list_push_front(g_list_t *list, void *element)
{
    list->list.push_front(element);
}

/* ---------------- g_list_pop_back() ---------------- */
void g_list_pop_back(g_list_t *list)
{
    list->list.pop_back();
}

/* ---------------- g_list_pop_front() ---------------- */
void g_list_pop_front(g_list_t *list)
{
    list->list.pop_front();
}

/* ---------------- g_list_begin() ---------------- */
g_iterator_t *g_list_begin(g_list_t *list)
{
    g_list_iterator_t *listIter = g_list_iterator_new(list);
    listIter->it = list->list.begin();

    return (g_iterator_t*)listIter;
}

/* ---------------- g_list_end() ---------------- */
g_iterator_t *g_list_end(g_list_t *list)
{
    g_list_iterator_t *listIter = g_list_iterator_new(list);
    listIter->it = list->list.end();

    return (g_iterator_t*)listIter;
}

/* ---------------- g_list_erase() ---------------- */
g_iterator_t *g_list_erase(g_list_t *list, g_iterator_t *it)
{
    
    g_list_iterator_t *listIter = (g_list_iterator_t*)it;

    listIter->it = list->list.erase(listIter->it);

    return it;
}

/* ---------------- g_list_empty() ---------------- */
int g_list_empty(g_list_t *list)
{
    if ( list->list.empty() )
        return 1;
    else
        return 0;
}

